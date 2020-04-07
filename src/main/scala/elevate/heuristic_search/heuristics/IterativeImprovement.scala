package elevate.heuristic_search.heuristic

import elevate.heuristic_search.util.Path
import elevate.heuristic_search.{Heuristic, HeuristicPanel}

class IterativeImprovement[P] extends Heuristic[P] {

  def start(panel:HeuristicPanel[P], initialSolution:P, depth:Int): (P, Option[Double], Path[P]) = {
    var solution:P = initialSolution
    val path = new Path(solution, panel.f(solution))

    var oldSolution = solution
    var i = 0
    do {
      i = i + 1
      oldSolution = solution

      //get neighbourhood
      val Ns = panel.N(solution)

      //evaluate neighbourhood
      Ns.foreach(ns => {
        (panel.f(ns._1), panel.f(solution)) match {
          case (Some(fns), Some(fsolution)) =>
            if (fns < fsolution) {
              solution = ns._1
              path.add(ns._1, ns._2, panel.f(ns._1))
            }
          case _ =>
        }
      })

    } while((panel.f(solution), panel.f(oldSolution)) match {
          case (Some(value0), Some(value1)) => (panel.f(solution).get < panel.f(oldSolution).get)
          case _ => false
        }
    )

    (solution, panel.f(solution), path)
  }
}



