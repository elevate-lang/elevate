package elevate.heuristic_search.heuristics

import elevate.heuristic_search._
import elevate.heuristic_search.util.{Solution, hashProgram}
//import elevate.heuristic_search.util.{Path, PathElement}

import scala.collection.immutable.Queue

// TODO: rename to breadth first search
class Exhaustive[P] extends Heuristic[P] {

  private val allowInvalid: Boolean = false

  def start(panel: HeuristicPanel[P], initialSolution: Solution[P], depth: Int, samples: Int): ExplorationResult[P] = {

    var counter: Int = 0
    var solution: Solution[P] = initialSolution
    var solutionValue: Option[Double] = panel.f(solution)
    var queue: Queue[(Int, Solution[P])] = Queue.empty[(Int, Solution[P])]
    queue = queue.enqueue(0, solution)

    while (!queue.isEmpty) {

      // get element from queue
      val current = queue.dequeue
      queue = current._2

      // execute all elements in the neighborhood and queue them
      val Ns = panel.N(current._1._2)
      Ns.foreach(ne => {

        val layer = current._1._1 + 1
        if (counter < samples) {

          // execute
          val fne = panel.f(ne)
          counter += 1

          // check result, minimum and queue accordingly
          fne match {

            // invalid: no performance value
            case None =>

              // only enqueue if we allow invalid solutions
              if (allowInvalid) {
                if (layer < depth) {
                  queue = queue.enqueue((layer, ne))
                }
              }

            // valid: performance value
            case Some(candidateValue) => {

              // check if a new minimum was found
              val update = solutionValue match {
                case Some(sValue) =>
                  candidateValue < sValue match {
                    case true => (ne, Some(candidateValue))
                    case false => (solution, Some(sValue))
                  }
                case None => (ne, Some(candidateValue))
              }
              solution = update._1
              solutionValue = update._2

              // enqueue if we have not reached the exploration limit
              if (layer < depth) {
                queue = queue.enqueue((layer, ne))
              }
            }
          }

        } else {
          return ExplorationResult(
            solution,
            solutionValue,
            None
          )
        }
      })
    }

    // return best found solution
    ExplorationResult(
      solution,
      solutionValue,
      None
    )
  }
}
