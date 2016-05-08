package lt.mif.vu.maly0541.stm

class Car(val direction: Direction.Value, val number: Int)

object Direction extends Enumeration {
  type Direction = Value
   val RIGHT = Value(0);
   val LEFT = Value(1);
} 