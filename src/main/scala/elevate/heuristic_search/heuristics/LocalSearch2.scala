package elevate.heuristic_search.heuristics

import elevate.core.Strategy
import elevate.heuristic_search._
import elevate.heuristic_search.util.Solution

import scala.util.Random

class LocalSearch2[P] extends Heuristic[P] {

  // check: do not terminate here if budget is left over
  // check: start from beginning, but do not make similar rewrites
  // check: maybe save history to access performance instead of execution?
  // todo: test

  import scala.collection.mutable

  def start(panel: HeuristicPanel[P], initialSolution: Solution[P], depth: Int, samples: Int): ExplorationResult[P] = {
    var solution: Solution[P] = initialSolution
    val initialSolutionValue: Option[Double] = panel.f(solution)
    var solutionValue: Option[Double] = initialSolutionValue
    var counter = 1 // Start with 1 to account for the initial solution evaluation
    // cache that identifies a rewrite by its rewrite sequence (Strategy and Location)
    val cache = mutable.Map[Seq[(Strategy[P], Int)], Option[Double]](solution.rewrite_sequence() -> initialSolutionValue)
    val tabu = mutable.Set.empty[Seq[(Strategy[P], Int)]]

    do {
      // Get neighborhood and ensure we don't exceed the sample limit
      val Ns = Random.shuffle(panel.N(solution).filterNot(elem => tabu.contains(elem.rewrite_sequence()))).take(samples - counter)

      // if no neighbor is left add this to tabu list and repeat from initial solution
      if (Ns.isEmpty || Ns.forall(ns => tabu.contains(ns.rewrite_sequence()))) {

        // add solution to tabu list
        tabu.add(solution.rewrite_sequence())

        // start from beginning
        solution = initialSolution
        solutionValue = initialSolutionValue
      } else {


        // Evaluate the neighborhood using the cache to avoid re-evaluations
        val betterNeighbor = Ns
          .flatMap { ns =>
            if (counter < samples) {
              cache.getOrElseUpdate(ns.rewrite_sequence(), {
                // If ns is not in cache, evaluate and cache it
                val value = panel.f(ns)
                counter += 1
                value
              }).map(fns => (ns, fns))
            } else {
              // If the sample limit is reached, skip further evaluations
              None
            }
          }
          .minByOption(_._2)

        // Check if a better solution was found
        betterNeighbor match {
          // If so, choose this as the new solution
          case Some((bestNeighbor, bestValue)) if bestValue < solutionValue.getOrElse(Double.MaxValue) =>
            solution = bestNeighbor
            solutionValue = Some(bestValue)
          case _ =>
            // If not, restart from the initial solution if budget is left

            // do not sample terminal expression again
            tabu.add(solution.rewrite_sequence())

            if (counter < samples) {
              solution = initialSolution
              solutionValue = initialSolutionValue
            } else {
              // Otherwise, terminate
              return ExplorationResult(solution, solutionValue, None)
            }
        }
      }

    } while (counter < samples)

    ExplorationResult(
      solution,
      solutionValue,
      None
    )
  }

}
