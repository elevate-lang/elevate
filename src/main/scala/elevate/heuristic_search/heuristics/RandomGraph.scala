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

      for (_ <- Range(0, depth)) {

        //get neighbourhood
        val Ns: Seq[Solution[P]] = panel.N(solution)

        // choose solution from neighborhood
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

            // get next element
            solution = Ns.apply(random.nextInt(Ns.size))
            solutionValue = panel.f(solution)
            sampleCounter += 1

            solution
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



