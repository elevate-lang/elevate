package elevate.heuristic_search.heuristic

import elevate.heuristic_search.util.Path
import elevate.heuristic_search.{Heuristic, ProblemConstraints}

class IterativeImprovement[P](var solution:P, val panel:ProblemConstraints[P]) extends Heuristic[P] {


  def start(): P = {
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
    } while (panel.f(solution).get < panel.f(oldSolution).get)

  path.printPathConsole()
    // make path part of settings
    // create folder as well, maybe use relative paths
    path.writePathToDot("/home/jo/developement/rise-lang/exploration/iterativeImprovement.dot")

    solution
  }
}



