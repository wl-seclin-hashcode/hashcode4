package hashcode

case class Junction(lat: Float, long: Float, id: Int) {
}

case class Street(
    junction1: Junction,
    junction2: Junction,
    bidir: Boolean,
    cost: Int,
    length: Int) {
  def ratio = (length + 0.0) / cost

  def joins(j1: Junction, j2: Junction) =
    (j1 == junction1 && j2 == junction2) ||
      (j2 == junction1 && j1 == junction2 && bidir)
}

case class Problem(
    junctions: List[Junction],
    streets: List[Street],
    nbCars: Int,
    virtualTime: Int,
    initialJunction: Int) {

  val allStreets = streets.flatMap {
    case s if s.bidir ⇒ List(s, Street(s.junction2, s.junction1, s.bidir, s.cost, s.length))
    case s            ⇒ List(s)
  }

  val neighbours = allStreets.groupBy(_.junction1) withDefaultValue Nil
}

