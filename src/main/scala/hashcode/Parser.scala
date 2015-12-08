package hashcode

object Parser {
  def read(): Problem = {
    val input = io.Source.fromFile("input/data.in").getLines.toList
    val first = input.head
    val Array(nbJunctions, nbStreets, virtualTime, nbCars, initialJunction) = first.split(" ").map(_.toInt)

    val junctions = for {
      (line, i) <- input.tail.take(nbJunctions).zipWithIndex
      Array(lat, long) = line.split(" ")
    } yield Junction(lat.toFloat, long.toFloat, i + 1)

    val streets = for {
      (line, i) <- input.tail.drop(nbJunctions).take(nbStreets).zipWithIndex
      Array(j1, j2, direction, cost, length) = line.split(" ")
      jun1 <- junctions.find(_.id == j1.toInt)
      jun2 <- junctions.find(_.id == j2.toInt)
      bidir = direction == 2
    } yield Street(jun1, jun2, bidir, cost.toInt, length.toInt)

    Problem(junctions, streets, nbCars, virtualTime, initialJunction)
  }
}