package hashcode

import scala.annotation.tailrec
import scala.util.Success
import scala.util.Try

object Validator {
  def score(solution: Solution, problem: Problem): Try[Int] = Try {
    import problem._
    def streetsUsed(segments: List[Int]) = for {
      (a, b) <- (1 :: segments.init) zip segments
      ja <- junctions.find(_.id == a)
      jb <- junctions.find(_.id == b)
      street <- streets.find(_.joins(ja, jb)).orElse {
        throw new Exception(s"no street from $ja to $jb")
      }
    } yield street

   val usedStrets= solution.cars.flatMap(r => streetsUsed(r.segments).toSet)
   usedStrets.map(_.length).sum 
  }

}