package hashcode

import scala.util.{Failure, Success}

object Main extends App {
  val problem = Parser.read()
  problem.streets.take(100).foreach(println)
  val solution = Solver.solve(problem)

  Validator.score(solution, problem) match {
    case Success(score) =>
      println(s"score : $score")
      Formatter.write(solution, score)
      Formatter.writeSvg(solution, problem, score)
    case Failure(e) =>
      e.printStackTrace()
  }

}