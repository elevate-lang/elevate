package elevate.heuristic_search

import elevate.core.Strategy
import elevate.heuristic_search.util.Solution

//used to implement heuristics
trait HeuristicPanel[P] {

  //get neighborhood of solution
  def N(solution: Solution[P]): Seq[Solution[P]]

  //get function value of solution
  def f(solution: Solution[P]): Option[Double]

  // get solutiosn
  def getSolution(solution: Solution[P], numbers: Seq[Int]): Option[Solution[P]]

  def checkRewrite(solution: Solution[P], rewrite: Int): Boolean

  def importSolution(filename: String): Solution[P]

  def exportSolution(solution: Solution[P], filename: String): Unit


}




