package hashcode

case class Junction(lat: Float, long: Float, id: Int) {
}

case class Street(
  junction1: Junction,
  junction2: Junction,
  bidir: Boolean,
  cost: Int,
  length: Int)

case class Problem(
    junctions: List[Junction], 
    streets: List[Street],
    nbCars:Int,
    virtualTime:Int, 
    initialJunction:Int)

