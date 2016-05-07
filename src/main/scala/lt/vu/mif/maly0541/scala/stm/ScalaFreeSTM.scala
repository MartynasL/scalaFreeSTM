package lt.vu.mif.maly0541.scala.stm

import scala.concurrent.stm._
import scala.concurrent.stm.TSet
import scalaz._
import scalaz.Free.liftFC
import scalaz.Free.runFC
import scalaz.Scalaz._
import scalaz.effect.IO

object ScalaFreeSTM {
  //Variable definitions
  final class SafeSet[A](private[ScalaFreeSTM] val tSet: TSet[A]) {
    def add(value: A)(implicit txn: InTxn): Unit = {
      tSet.+=(value)
    }

    def remove(value: A)(implicit txn: InTxn): Unit = {
      tSet.-=(value)
    }
  }

  final class SafeVal[A](private[ScalaFreeSTM] val tRef: Ref[A])

  //Operation definitions
  sealed trait STMOp[A]
  private object STMOp {
    //set operations
    case class NewTSet[A]() extends STMOp[SafeSet[A]]
    case class AddTSet[A](value: A, set: SafeSet[A]) extends STMOp[Unit]
    case class RemoveTSet[A](value: A, set: SafeSet[A]) extends STMOp[Unit]
    case class CountTSet[A](set: SafeSet[A]) extends STMOp[Int]

    //one value variable operations
    case class NewTVal[A](value: A) extends STMOp[SafeVal[A]]
    case class ReadTVal[A](tVal: SafeVal[A]) extends STMOp[A]
    case class WriteTVal[A](value: A, tVal: SafeVal[A]) extends STMOp[Unit]

    //basic operations
    case object Retry extends STMOp[Unit]
  }
  import STMOp._

  type STMCoyoneda[A] = Coyoneda[STMOp, A]
  type FreeSTM[A] = Free[STMCoyoneda, A]

  def newTSet[A](): FreeSTM[SafeSet[A]] = liftFC[STMOp, SafeSet[A]](NewTSet())
  def addTSet[A](value: A, set: SafeSet[A]): FreeSTM[Unit] = liftFC[STMOp, Unit](AddTSet(value, set))
  def removeTSet[A](value: A, set: SafeSet[A]): FreeSTM[Unit] = liftFC[STMOp, Unit](RemoveTSet(value, set))
  def countTSet[A](set: SafeSet[A]): FreeSTM[Int] = liftFC[STMOp, Int](CountTSet(set))

  def newTVal[A](value: A): FreeSTM[SafeVal[A]] = liftFC[STMOp, SafeVal[A]](NewTVal(value))
  def readTVal[A](tVal: SafeVal[A]): FreeSTM[A] = liftFC[STMOp, A](ReadTVal(tVal))
  def writeTVal[A](value: A, tVal: SafeVal[A]): FreeSTM[Unit] = liftFC[STMOp, Unit](WriteTVal(value, tVal))

  val retry: FreeSTM[Unit] = liftFC[STMOp, Unit](Retry)

  //Interpreter
  private type InTxnInterpreter[A] = InTxn => A
  private val interpretOp: STMOp ~> InTxnInterpreter =
    new (STMOp ~> InTxnInterpreter) {
      def apply[A](op: STMOp[A]): InTxnInterpreter[A] = {
        op match {
          //Set interpreting
          case NewTSet()              => { implicit txn => new SafeSet(TSet()) }
          case AddTSet(value, set)    => { implicit txn => set.add(value) }
          case RemoveTSet(value, set) => { implicit txn => set.remove(value) }
          case CountTSet(set)         => { implicit txn => set.tSet.size }
          case Retry                  => { implicit txn => scala.concurrent.stm.retry }

          //Val interpreting
          case NewTVal(value)         => { implicit txn => new SafeVal(Ref(value)) }
          case ReadTVal(tVal)         => { implicit txn => tVal.tRef() }
          case WriteTVal(value, tVal) => { implicit txn => tVal.tRef() = value }
        }
      }
    }

  def runTransaction[A](trans: FreeSTM[A]) {
      atomic {
        runFC[STMOp, InTxnInterpreter, A](trans)(interpretOp)
      }    
  }
  
  def checkAndRun(expression: Boolean, f: FreeSTM[Unit]): FreeSTM[Unit] = {
    expression ? ().point[FreeSTM] | f
  }
}