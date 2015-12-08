package hashcode

import scala.annotation.tailrec
import scala.util.Success
import scala.util.Try

object Validator {
  def score(solution: Solution, problem: Problem): Try[Int] = Try {
    val usedStrets = solution.streetsUsed(problem).flatten.distinct
    usedStrets.map(_.length).sum
  }

}