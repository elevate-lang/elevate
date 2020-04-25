package elevate.heuristic_search.heuristic

import elevate.heuristic_search.util.Path
import elevate.heuristic_search.{Heuristic, HeuristicPanel}

class IterativeImprovement[P] extends Heuristic[P] {

  def start(panel:HeuristicPanel[P], initialSolution:P, depth:Int): (P, Option[Double], Path[P]) = {
    var solution:P = initialSolution
    var solutionValue:Option[Double] = panel.f(solution)
    val path = new Path(solution, solutionValue)

    var oldSolution = solution
    var oldSolutionValue:Option[Double] = solutionValue
    var i = 0
    do {
      i = i + 1
      // save current state
      oldSolution = solution
      oldSolutionValue = solutionValue

      //get neighbourhood
      val Ns = panel.N(solution)

      //evaluate neighbourhood
      Ns.foreach(ns => {
        val fns = panel.f(ns._1)
        val fsolution = solutionValue
        (fns, fsolution) match {
          case (Some(fnsInternal), Some(fsolutionInternal)) =>

            // check for new minimum
            if (fnsInternal < fsolutionInternal) {
              solution = ns._1
              solutionValue = fns

              path.add(ns._1, ns._2, fns)
            }
          case _ =>
        }
      })

      // check, if chosen solution is better
    } while((solutionValue, oldSolutionValue) match {
          case (Some(value0), Some(value1)) => (solutionValue.get < oldSolutionValue.get)
          case _ => false
        }
    )

    (solution, solutionValue, path)
  }
}



