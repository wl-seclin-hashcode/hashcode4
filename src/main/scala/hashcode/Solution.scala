package hashcode

case class Solution(cars: List[Route]) {

  def streetsUsed(problem: Problem): List[List[Street]] =
    cars.map(r => streetsUsed(problem, r.segments))

  def streetsUsed(problem: Problem, segments: List[Int]): List[Street] = {
    import problem._
    if (segments.isEmpty) Nil
    else for {
      (a, b) <- (0 :: segments.init) zip segments
      ja <- junctions.find(_.id == a)
      jb <- junctions.find(_.id == b)
      street <- streets.find(_.joins(ja, jb)).orElse {
        throw new Exception(s"no street from $ja to $jb")
      }
    } yield street
  }

}

case class Route(car: Int, segments: List[Int])