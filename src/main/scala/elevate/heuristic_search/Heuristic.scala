package elevate.heuristic_search

import elevate.heuristic_search.util.{SearchSpace, Solution}

trait Heuristic[P]{

  def start(panel:HeuristicPanel[P], solution:Solution[P], depth: Int):(P, Option[Double], SearchSpace[P])

  //get metadata in future
}





