package elevate

import heuristic_search.util._

package object heuristic_search {

  case class ExplorationResult[P](
                                   solution: Solution[P], // resulting solution
                                   performance: Option[Double], // solutions performance
                                   searchSpace: Option[SearchSpace[P]] // search space (path, embedding, tree)
                                 ) {
    override def toString: String = {
      s"""
         |ExplorationResult:
         |Solution (exp): ${hashProgram(solution.expression)}
         |Solution (exp + strat): ${hashProgram(solution)}
         |Strategies: ${solution.strategies.mkString("[", ", ", "]")}
         |Performance: ${performance}
         |Expression: \n ${solution.expression}
         |
         |""".stripMargin
    }
  }

  sealed trait HeuristicPanelChoice

  case object StandardPanelChoice extends HeuristicPanelChoice

  case object SimpleRewritePanelChoice extends HeuristicPanelChoice

}
