package elevate.heuristic_search.heuristics

import elevate.heuristic_search._
import elevate.heuristic_search.util.Solution
import scala.collection.mutable.ListBuffer

class RandomGraph[P] extends Heuristic[P] {

  def start(panel: HeuristicPanel[P], initialSolution: Solution[P], depth: Int, samples: Int): ExplorationResult[P] = {
    var solution: Solution[P] = initialSolution
    var solutionValue: Option[Double] = panel.f(solution)
    //    var min: Option[Double] = solutionValue

    val random = scala.util.Random

    // later:
    // don't allow duplicates
    // save position and action

    // todo: limit rewrite depth
    // (number of rewrites applied in total (collapse rules+inverse? -> how to recognise?)

    for (_ <- Range(0, samples)) {
      //get neighbourhood
      val Ns: Seq[Solution[P]] = panel.N(solution)

      solution = Ns.size match {
        case 0 =>
          // neighborhood emtpy -> stop search
          println("empty neighborhood - this should not happen with bidirectional rules")
          return ExplorationResult(
            solution,
            solutionValue,
            None
          )

        // chose valid solution randomly from neighborhood
        case _ =>

          // get permutation for
          val permutationIterator = generateRandomPermutation(Ns.size, random).iterator

          var found = false
          while (!found) {

            // get next element
            solution = Ns.apply(permutationIterator.next())

            found = panel.f(solution) match {
              // invalid candidate
              case None =>
                false

              // valid candidate
              case Some(value) =>
                solutionValue = Some(value)
                true
            }
          }

          // return chosen solution
          solution
      }
    }

    ExplorationResult(
      solution,
      solutionValue,
      None
    )
  }

  def generateRandomPermutation(n: Int, random: scala.util.Random): List[Int] = {
    val buffer = ListBuffer.range(0, n)

    for (i <- n - 1 to 1 by -1) {
      val j = random.nextInt(i + 1)
      val temp = buffer(i)
      buffer(i) = buffer(j)
      buffer(j) = temp
    }

    buffer.toList
  }

}



