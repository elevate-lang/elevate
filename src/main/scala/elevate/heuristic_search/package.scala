package elevate

import heuristic_search.util._

package object heuristic_search {

  case class ExplorationResult[P](
                                   solution: Solution[P], // resulting solution
                                   performance: Option[Double], // solutions performance
                                   searchSpace: Option[SearchSpace[P]] // search space (path, embedding, tree)
                                 ) {
    override def toString: String = {
      // print information here
      "ExplorationResult: "
    }
  }


  sealed trait HeuristicPanelChoice

  case object StandardPanelChoice extends HeuristicPanelChoice

  case object SimpleRewritePanelChoice extends HeuristicPanelChoice


}
