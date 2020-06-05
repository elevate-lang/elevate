package elevate.heuristic_search

trait Runner[P] {
  def execute(solution: P):(P,Option[Double])
}
