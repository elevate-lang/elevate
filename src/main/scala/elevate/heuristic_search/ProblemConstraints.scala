package elevate.heuristic_search

import elevate.core.Strategy

//used to implement heuristics
trait ProblemConstraints[P] {

  //get neighborhood of solution
  def N(solution:P):Set[(P, Strategy[P])]

  //get function value of solution
  def f(solution:P):Option[Double]

}




