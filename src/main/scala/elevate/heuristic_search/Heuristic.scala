package elevate.heuristic_search

import elevate.heuristic_search.util.{SearchSpace, Solution}

trait Heuristic[P] {

  def start(panel: HeuristicPanel[P], solution: Solution[P], depth: Int): ExplorationResult[P]

  //get metadata in future
}





