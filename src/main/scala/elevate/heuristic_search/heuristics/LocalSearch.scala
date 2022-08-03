package elevate.heuristic_search.heuristics

import elevate.heuristic_search._
import elevate.heuristic_search.util.Solution

class LocalSearch[P] extends Heuristic[P] {

  def start(panel: HeuristicPanel[P], initialSolution: Solution[P], depth: Int, samples: Int): ExplorationResult[P] = {
    var solution: Solution[P] = initialSolution
    var solutionValue: Option[Double] = panel.f(solution)

    var oldSolution = solution
    var oldSolutionValue: Option[Double] = solutionValue
    var i = 0
    var l = 0
    do {
      i = i + 1
      // save current state
      oldSolution = solution
      oldSolutionValue = solutionValue

      //get neighbourhood
      val Ns = panel.N(solution)

      //evaluate neighbourhood
      println("\n \n \n LAYER: " + l)
      println("Ns.size: " + Ns.size)
      l += 1
      Ns.foreach(ns => {
        val fns = panel.f(ns)
        val fsolution = solutionValue

        (fns, fsolution) match {
          case (Some(fnsInternal), Some(fsolutionInternal)) =>

            // check for new minimum
            if (fnsInternal < fsolutionInternal) {
              solution = ns
              solutionValue = fns
            }
          case _ =>
        }
      })

    } while (
      ((solutionValue, oldSolutionValue) match {
        case (Some(value0), Some(value1)) => (solutionValue.get < oldSolutionValue.get)
        case _ => false
      })
    )

    ExplorationResult(
      solution,
      solutionValue,
      None
    )
  }
}



