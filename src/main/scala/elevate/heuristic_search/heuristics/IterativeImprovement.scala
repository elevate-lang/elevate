package elevate.heuristic_search.heuristic

import elevate.core.Strategy
import elevate.heuristic_search.util.Path
import elevate.heuristic_search.{Heuristic, HeuristicPanel}
import elevate.heuristic_search.util.Solution

class IterativeImprovement[P] extends Heuristic[P] {

  def start(panel:HeuristicPanel[P], initialSolution:Solution[P], depth:Int): (P, Option[Double], Path[P]) = {
//    var solution:P = initialSolution
    var solution = initialSolution
    var solutionValue:Option[Double] = panel.f(solution)
    var solutionStrategies = Seq.empty[Strategy[P]]
    val path = new Path(solution.expression, solutionValue, null, null, 0)

    var oldSolution = solution
    var oldSolutionValue:Option[Double] = solutionValue
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

        // add every node to path
        // todo mark chosen ones (aka solutions)

        val current = path.current
        // test this node, so add this node to the path
        path.add(ns.expression, ns.strategies.last, fns)


        (fns, fsolution) match {
          case (Some(fnsInternal), Some(fsolutionInternal)) =>

            // check for new minimum
            if (fnsInternal < fsolutionInternal) {
              solution = ns
              solutionValue = fns
              solutionStrategies = ns.strategies

//              path.add(ns.expression, ns.strategies.last, fns)
            }
          case _ =>
        }

        // after testing go one node back
        path.add(current.program, elevate.core.strategies.basic.revert, current.value)
      })

      // add chosen solution to path

      val solutionStrategy:Strategy[P] = solutionStrategies.size match{
        case 0 => elevate.core.strategies.basic.id
        case _ => solutionStrategies.last
      }

//      solution.equals(oldSolution) match {
//        case true => path.add(solution.expression, solutionStrategy, solutionValue)
//        case false => path.add(solution.expression, solutionStrategy, solutionValue)
//      }

    path.add(solution.expression, solutionStrategy, solutionValue)

      // check, if chosen solution is better
    } while((solutionValue, oldSolutionValue) match {
          case (Some(value0), Some(value1)) => (solutionValue.get < oldSolutionValue.get)
          case _ => false
        }
    )

    (solution.expression, solutionValue, path)
  }
}



