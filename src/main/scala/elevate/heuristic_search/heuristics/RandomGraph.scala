package elevate.heuristic_search.heuristics

import elevate.heuristic_search._
import elevate.heuristic_search.util.Solution

class RandomGraph[P] extends Heuristic[P] {

  def start(panel: HeuristicPanel[P], initialSolution: Solution[P], depth: Int, samples: Int): ExplorationResult[P] = {
    var solution: Solution[P] = initialSolution
    var solutionValue: Option[Double] = panel.f(solution)
    //    var min: Option[Double] = solutionValue

    val random = scala.util.Random

    // later:
    // don't allow duplicates
    // save position and action

    for (_ <- Range(0, samples)) {
      //get neighbourhood
      val Ns: Seq[Solution[P]] = panel.N(solution)
      solution = Ns.size match {
        case 0 => println("empty neighborhood - this should not happend")
          return ExplorationResult(
            solution,
            solutionValue,
            None
          )
        case _ => Ns(random.nextInt(Ns.size))
      }
      solutionValue = panel.f(solution)

      //       check if new min was found
      //      min = solutionValue match {
      //        case Some(value) => value < min match {
      //          case true => solutionValue
      //          case false => min
      //        }
      //        case None => min
      //      }
    }

    ExplorationResult(
      solution,
      solutionValue,
      None
    )
  }
}



