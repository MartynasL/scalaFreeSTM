package scalaFreeSTM.scalaFreeSTM

import scala.concurrent.stm.TSet
import scalaz.Free.{ liftFC, runFC }
import scalaz.Free
import scalaz.Coyoneda

object ScalaFreeSTM {
  final class SafeSet[A](private val set: TSet[A])

  sealed trait STMOp[A];
  private object STMOp {
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
  def addTSet[A](value: A, set: SafeSet[A]) : FreeSTM[Unit] = liftFC[STMOp, Unit](AddTSet(value, set))
  def removeTSet[A](value: A, set: SafeSet[A]) : FreeSTM[Unit] = liftFC[STMOp, Unit](RemoveTSet(value, set))
  def countTSet[A](set: SafeSet[A]) : FreeSTM[Int] = liftFC[STMOp, Int](CountTSet(set))
  def retry = liftFC[STMOp, Unit](Retry)
}