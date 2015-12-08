package hashcode


object Solver {

  type Path = List[Street]

  def findStreet(junction: Junction, problem:Problem, visited: Set[Street], currentTime: Int) = {
    val neighbours = problem.neighbours(junction)
    val availableStreets = neighbours diff visited filter (_.cost + currentTime <= problem.virtualTime)
    val avStreets2 = if (availableStreets.isEmpty)
      neighbours filter (_.cost + currentTime <= problem.virtualTime)
    else availableStreets
    if (avStreets2.isEmpty) None else Some(avStreets2.maxBy(_.ratio))
  }

  def findPath(junction: Junction, problem: Problem, visited: Set[Street], currentTime: Int, path: List[Street]): List[Street] = {
    findStreet(junction, problem, visited, currentTime) match {
      case None ⇒ path.reverse
      case Some(street) ⇒ findPath(street.junction2, problem, visited + street, currentTime + street.cost, street::path)
    }
  }

  def solve(problem: Problem): Solution = {
    val pathList = (0 until problem.nbCars).foldLeft(List.empty[Path]) {
      (path, _) ⇒ findPath(problem.initJunction, problem, path.flatten.toSet, 0, List.empty[Street]) :: path
    }

    Solution(pathList.zipWithIndex map {
      case (path, i) ⇒ Route(i, path map (_.junction2.id) )
    })
  }


}