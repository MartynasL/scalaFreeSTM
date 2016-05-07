package lt.vu.mif.maly0541.scala.stm

import scala.concurrent.stm._
import scala.concurrent.stm.TSet

import scalaz._
import scalaz.Free.liftFC
import scalaz.Free.runFC
import scalaz.Scalaz._

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

  //Operation definitions
  sealed trait STMOp[A]
  private object STMOp {
    //set operations
    case class NewTSet[A]() extends STMOp[SafeSet[A]]
    case class AddTSet[A](value: A, set: SafeSet[A]) extends STMOp[Unit]
    case class RemoveTSet[A](value: A, set: SafeSet[A]) extends STMOp[Unit]
    case class CountTSet[A](set: SafeSet[A]) extends STMOp[Int]
    case object Retry extends STMOp[Unit]    
  }
  import STMOp._

  type STMCoyoneda[A] = Coyoneda[STMOp, A]
  type FreeSTM[A] = Free[STMCoyoneda, A]

  def newTSet[A](): FreeSTM[SafeSet[A]] = liftFC[STMOp, SafeSet[A]](NewTSet())
  def addTSet[A](value: A, set: SafeSet[A]): FreeSTM[Unit] = liftFC[STMOp, Unit](AddTSet(value, set))
  def removeTSet[A](value: A, set: SafeSet[A]): FreeSTM[Unit] = liftFC[STMOp, Unit](RemoveTSet(value, set))
  def countTSet[A](set: SafeSet[A]): FreeSTM[Int] = liftFC[STMOp, Int](CountTSet(set))
  val retry: FreeSTM[Unit] = liftFC(Retry)

  //Interpreter
  private type InTxnReader[A] = InTxn => A
  private val interpOp: STMOp ~> InTxnReader =
    new (STMOp ~> InTxnReader) {
      def apply[A](op: STMOp[A]): InTxnReader[A] = {
        op match {
          case NewTSet()              => { implicit txn => new SafeSet(TSet()) }
          case AddTSet(value, set)    => { implicit txn => set.add(value) }
          case RemoveTSet(value, set) => { implicit txn => set.remove(value) }
          case CountTSet(set)         => { implicit txn => set.tSet.size }
          case Retry                  => { implicit txn => scala.concurrent.stm.retry }
        }
      }
    }
}