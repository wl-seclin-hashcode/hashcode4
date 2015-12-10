package hashcode

import java.io.PrintStream
import java.io.PrintWriter

object Formatter {
  def write(solution: Solution, score: Int): Unit = {
    val name = s"out.${score}.txt"
    val f = new PrintStream(name)
    for {
      car <- solution.cars
    } {
      f.println(car.segments.size + 1)
      f.println(1)
      for {
        segment <- car.segments
      } f.println(segment)
    }
    f.close
    println(s"wrote to $name")
  }

  def writeSvg(solution: Solution, problem: Problem, score: Int) {
    val routes = solution.streetsUsed(problem)
    val minLat = problem.junctions.minBy(_.lat).lat
    val maxLat = problem.junctions.maxBy(_.lat).lat
    val minLong = problem.junctions.minBy(_.long).long
    val maxLong = problem.junctions.maxBy(_.long).long
    def norm(coord: Float, min: Float, max: Float, length: Int): Int = {
      length * (coord - min) / (max - min)
    }.toInt

    def toPoint(junc: Junction): String = {
      val x = norm(junc.lat, minLat, maxLat, 800)
      val y = norm(junc.long, minLong, maxLong, 600)
      s"$x,$y "
    }

    def streetToPoints(street: Street): String =
      toPoint(street.junction1) + toPoint(street.junction2)

    val colors = List("red", "blue", "black", "green", "yellow", "grey", "orange", "lime")

    def carToPolygon(streets: List[Street], color: Int) = {
      val junctions = streets.headOption.map(_.junction1).toList ::: streets.map(_.junction2)
      val points = junctions.map(toPoint).mkString
      <polygon points={ points } style={ s"fill:none;stroke:${colors(color % colors.size)};stroke-width:1" }/>
    }

    def svg =
      <html>
        <body>
          <svg height="600" width="800">
            {
              for {
                (streets, i) <- routes.zipWithIndex
              } yield carToPolygon(streets.distinct, i)
            }
          </svg>
        </body>
      </html>

    val name = s"out.${score}.html"
    xml.XML.save(name, svg)
    println(s"wrote to $name")

  }
}