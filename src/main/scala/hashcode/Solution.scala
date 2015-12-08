package hashcode

case class Solution(cars: List[Route])

case class Route(car: Int, segments: List[Segment])

case class Segment(from: Junction, to: Junction, street: Street)