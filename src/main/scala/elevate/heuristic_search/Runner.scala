package elevate.heuristic_search

import elevate.heuristic_search.util.Solution

trait Runner[P] {
//  def execute(solution: P):(P,Option[Double])
  def execute(solution: Solution[P]):(P,Option[Double])
}
