package hashcode

import java.io.PrintStream

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
}