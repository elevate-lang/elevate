package elevate.heuristic_search.heuristic

import elevate.core.Strategy
import elevate.heuristic_search.util.Path
import elevate.heuristic_search.{Heuristic, HeuristicPanel}
import elevate.heuristic_search.util.Solution

class IterativeImprovement[P] extends Heuristic[P]:

  def start(panel:HeuristicPanel[P], initialSolution:Solution[P], depth:Int): (P, Option[Double], Path[P]) =
//    var solution:P = initialSolution
    var solution = initialSolution
    var solutionValue:Option[Double] = panel.f(solution)
    val path = new Path(solution.expression, solutionValue)

    var oldSolution = solution
    var oldSolutionValue:Option[Double] = solutionValue
    var i = 0
    var l = 0
    while
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
        (fns, fsolution) match
          case (Some(fnsInternal), Some(fsolutionInternal)) =>
            // check for new minimum
            if fnsInternal < fsolutionInternal then
              solution = ns
              solutionValue = fns

              path.add(ns.expression, ns.strategies.last, fns)
          case _ =>
      })

      // check, if chosen solution is better
      (solutionValue, oldSolutionValue) match
        case (Some(value0), Some(value1)) => (solutionValue.get < oldSolutionValue.get)
        case _ => false
    do ()

    (solution.expression, solutionValue, path)
  end start

end IterativeImprovement
