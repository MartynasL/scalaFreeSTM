package lt.mif.vu.maly0541.stm

import lt.vu.mif.maly0541.scala.stm.ScalaFreeSTM
import lt.vu.mif.maly0541.scala.stm.ScalaFreeSTM._

object SingleLaneBridge {
  private val carsOnBridge: SafeSet[Car] = new SafeSet();
  private val bridgeDirection: SafeVal[Direction.Value] = new SafeVal(null: Direction.Value);

  private def arriveOnBridge(car: Car) {
    val atomicArrive = {
      for {
        carsCount <- countTSet(carsOnBridge)
        currentDirection <- readTVal(bridgeDirection)
        _ <- checkAndRunIfTrue((carsCount == 3) || (carsCount != 0 && car.direction.equals(currentDirection)), retry)
        _ <- checkAndRunIfTrue(carsCount == 0, writeTVal(car.direction, bridgeDirection))
        _ <- addTSet(car, carsOnBridge)
      } yield ()
    }
    runTransaction(atomicArrive)
  }

  private def exitBridge(car: Car) {
    var atomicExit = {
      for {
        _ <- removeTSet(car, carsOnBridge)
      } yield ()
    }
    runTransaction(atomicExit)
  }

  def crossBridge(car: Car) {
    arriveOnBridge(car);
    Thread.sleep(1000);
    exitBridge(car);
  }
}