package elevate.heuristic_search

trait Heuristic[P]{

  def start(panel:HeuristicPanel[P], solution:P, depth: Int):(P, Option[Double])

  //get metadata in future
}





