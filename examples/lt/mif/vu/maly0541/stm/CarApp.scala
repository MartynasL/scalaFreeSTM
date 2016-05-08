package lt.mif.vu.maly0541.stm

object CarApp {
  def main(args: Array[String]) {
    val cars = List(new Car(Direction.RIGHT, 1), new Car(Direction.RIGHT, 2), new Car(Direction.LEFT, 3)
    , new Car(Direction.RIGHT, 4), new Car(Direction.LEFT, 5));
    
//     val cars = List(new Car(Direction.EAST, 1), new Car(Direction.WEST, 2));
    
    cars.foreach { car =>
      {
        val thread = new Thread {
          override def run {
            SingleLaneBridge.crossBridge(car);
            println("car " + car.number + "crossed ")
          }
        }
        thread.start
        Thread.sleep(50)
      }
    }
  }
}