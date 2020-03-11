package elevate.heuristic_search.heuristics

import elevate.heuristic_search.{Heuristic, ProblemConstraints}

class IterativeImprovement[P](var solution:P, val panel:ProblemConstraints[P]) extends Heuristic[P] {

  def start(): P = {
    var oldSolution = solution

    do {
      oldSolution = solution

      //get neighbourhood
      val Ns = panel.N(solution)

      //evaluate neighbourhood
      println("Ns: " + Ns.size)
      Ns.foreach(ns => {
        (panel.f(ns), panel.f(solution)) match {
          case (Some(fns), Some(fsolution)) =>
            if (fns < fsolution) {
              solution = ns
            }
          case _ =>
        }
      })

    } while (panel.f(solution).get < panel.f(oldSolution).get)

    solution
  }
}



