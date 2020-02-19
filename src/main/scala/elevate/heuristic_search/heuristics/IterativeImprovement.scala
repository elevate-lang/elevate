package elevate.heuristic_search.heuristics

import elevate.heuristic_search.{Heuristic, ProblemConstraints}

class IterativeImprovement[P](var solution:P, val panel:ProblemConstraints[P]) extends Heuristic[P] {

  def start(): P = {
    var oldSolution = solution

    do {
      oldSolution = solution
      println("solution: " + panel.f(oldSolution))

      //get neighbourhood
      val Ns = panel.N(solution)

      //evaluate neighbourhood
      println("Ns: " + Ns.size)
      Ns.foreach(ns => {
        if (panel.f(ns) > 0 && panel.f(ns) < panel.f(solution)) {
          solution = ns
        }
      })

    } while (panel.f(solution) < panel.f(oldSolution))


    println("solution: " + panel.f(solution))

    solution
  }
}
