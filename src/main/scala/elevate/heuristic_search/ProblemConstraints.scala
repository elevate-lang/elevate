package elevate.heuristic_search

//used to implement heuristics
trait ProblemConstraints[P] {

  //get neighborhood of solution
  def N(solution:P):Set[P]

  //get function value of solution
  def f(solution:P):Double

}




