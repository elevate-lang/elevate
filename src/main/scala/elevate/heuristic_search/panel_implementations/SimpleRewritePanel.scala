package elevate.heuristic_search.panel_implementations

import elevate.core.Strategy
import elevate.heuristic_search.{HeuristicPanel, Runner}
import elevate.heuristic_search.util.Solution

// do we need elevate strategies here? No!

// todo implement class
class SimpleRewritePanel[P](
                             val runner: Runner[P],
                             val strategies: Set[Strategy[P]]
                           ) extends HeuristicPanel[P] {


  //  class StandardPanel[P](
  //                          val runner: Runner[P],
  //                          val strategies: Set[Strategy[P]],
  //                          val afterRewrite: Option[Strategy[P]] = None, // e.g. rewrite normal form
  //                          val beforeExecution: Option[Strategy[P]] = None, // e.g. code-gen normal form
  //                          val rewriter: Option[Solution[P] => Set[Solution[P]]] = None,
  //                          val importExport: Option[(String => Solution[P], (Solution[P], String) => Unit)]
  //                        ) extends HeuristicPanel[P] {

  override def N(solution: Solution[P]): Set[Solution[P]] = ???

  override def f(solution: Solution[P]): Option[Double] = ???

  override def getSolution(solution: Solution[P], numbers: Seq[Int]): Option[Solution[P]] = ???

  override def checkRewrite(solution: Solution[P], rewrite: Int): Boolean = ???

  override def importSolution(filename: String): Solution[P] = ???

  override def exportSolution(solution: Solution[P], filename: String): Unit = ???

}
