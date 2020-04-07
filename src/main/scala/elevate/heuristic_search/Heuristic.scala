package elevate.heuristic_search

import elevate.heuristic_search.util.Path

trait Heuristic[P]{

  def start(panel:HeuristicPanel[P], solution:P, depth: Int):(P, Option[Double], Path[P])

  //get metadata in future
}





