package elevate.heuristic_search

import elevate.core.Strategy
import elevate.heuristic_search.util.Solution

//used to implement heuristics
trait HeuristicPanel[P] {

  //get neighborhood of solution
  def N(solution:Solution[P]):Set[Solution[P]]

  //get function value of solution
  def f(solution:Solution[P]):Option[Double]

}




