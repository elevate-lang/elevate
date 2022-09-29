package elevate.heuristic_search.heuristics

import elevate.heuristic_search._
import elevate.heuristic_search.util.{Solution, hashProgram}

class LocalSearchGraph[P] extends Heuristic[P] {

  def start(panel: HeuristicPanel[P], initialSolution: Solution[P], depth: Int, samples: Int): ExplorationResult[P] = {
    var solution: Solution[P] = initialSolution
    var solutionValue: Option[Double] = panel.f(solution)
    var old_solution = solution
    //    var min: Option[Double] = solutionValue

    val random = scala.util.Random

    // later:
    // don't allow duplicates
    // save position and action

    do {
      //get neighbourhood
      val Ns: Seq[Solution[P]] = panel.N(solution)

      // evaluate neighborhood and get minimum
      old_solution = solution
      solution = Ns.reduceLeft((a, b) => min(panel, a, b))
      solutionValue = panel.f(solution)

      // end loop if no new element was found
    } while (!old_solution.equals(solution))

    ExplorationResult(
      solution,
      solutionValue,
      None
    )
  }

  private def min(panel: HeuristicPanel[P], a: Solution[P], b: Solution[P]): Solution[P] = {
    panel.f(a) match {
      case Some(f_a) => panel.f(b) match {
        case Some(f_b) if f_a < f_b => a
        case Some(f_b) => b
        case None => a
      }
      case None => panel.f(b) match {
        case Some(f_b) => b
        case None => a
      }
    }
  }

}



