package elevate.heuristic_search.heuristics

import elevate.heuristic_search._
import elevate.heuristic_search.util.Solution
import scala.collection.mutable.ListBuffer

class RandomGraph[P] extends Heuristic[P] {

  def start(panel: HeuristicPanel[P], initialSolution: Solution[P], depth: Int, samples: Int): ExplorationResult[P] = {

    var solution: Solution[P] = initialSolution
    var solutionValue: Option[Double] = panel.f(solution)
    val random = scala.util.Random
    var sampleCounter: Int = 0

    while (sampleCounter < samples) {

      // reset solution
      solution = initialSolution

      var depthCounter: Int = 0
      while (depthCounter < depth && sampleCounter < samples) {
        depthCounter += 1
        sampleCounter += 1

        //get neighbourhood
        val Ns: Seq[Solution[P]] = panel.N(solution)

        // choose solution from neighborhood
        Ns.size match {

          case 0 =>
            // empty neighborhood
            // abort this try and reset
            depthCounter = depth

          // choose valid solution randomly from neighborhood
          case _ =>

            var foundValid = false

            var attempts = scala.collection.mutable.Set.empty[Solution[P]]

            while (!foundValid) {

              // filter out already visited candidates
              val candidates = Ns.filter(sol => !attempts.contains(sol))

              // if no candidate left start from root
              if (candidates.isEmpty) {
                depthCounter = depth
                foundValid = true
              } else {
                // get next element
                sampleCounter += 1
                val candidate = candidates.apply(random.nextInt(candidates.size))
                val candidateValue = panel.f(candidate)

                candidateValue match {
                  case None =>
                    attempts += candidate
                  case Some(value) =>
                    foundValid = true
                    solution = candidate
                    solutionValue = candidateValue
                }
              }
            }
        }
      }
    }

    ExplorationResult(
      solution,
      solutionValue,
      None
    )
  }

  // Code to only allow valid programs
  //
  //  // get permutation for
  //  val permutationIterator = generateRandomPermutation(Ns.size, random).iterator
  //
  //  var found = false
  //  while (!found) {
  //
  //    // get next element
  //    solution = Ns.apply(permutationIterator.next())
  //    panel.f(solution)
  //
  //    found = panel.f(solution) match {
  //      // invalid candidate
  //      case None =>
  //        false
  //
  //      // valid candidate
  //      case Some(value) =>
  //        solutionValue = Some(value)
  //        true
  //    }
  //  }
  //
  //  // return chosen solution
  //  solution

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



