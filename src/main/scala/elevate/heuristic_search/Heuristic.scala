package elevate.heuristic_search

import elevate.heuristic_search.util.{Path, Solution}

trait Heuristic[P]:

  def start(panel:HeuristicPanel[P], solution:Solution[P], depth: Int):(P, Option[Double], Path[P])

  //get metadata in future
end Heuristic
