package elevate.heuristic_search.heuristics

import elevate.heuristic_search.util.{Path, Solution}
import elevate.heuristic_search.{ExplorationResult, Heuristic, HeuristicPanel}

class Annealing[P] extends Heuristic[P] {

  def start(panel: HeuristicPanel[P], initialSolution: Solution[P], depth: Int): ExplorationResult[P] = {
    var solution = initialSolution

    val path = new Path(solution.expression, panel.f(solution), null, null, 0)

    // initialization of helper
    val random = scala.util.Random
    var T = 10.0
    val alpha = 0.75

    // main loop
    var k = 0

    println("welcome to simulated annealing")

    while (k < depth) {
      println("layer: " + k)
      println("current: " + solution)
      k = k + 1
      // create neighborhood
      val Ns = panel.N(solution)

      // init solution and solutionCandidate runtimes
      val solutionRuntime = panel.f(solution)
      var solutionCandidateRuntime = solutionRuntime

      var validCandidate = false

      var solutionCandidate: Solution[P] = null
      solution
      // get new solution candidate
      while (!validCandidate) {
        //pick element from neighborhodd
        solutionCandidate = Ns.toSeq(random.nextInt(Ns.size))

        // rewrite
        // save state lambda
        val stateLambda = solution
        // execute
        solutionCandidateRuntime = panel.f(solutionCandidate)
        // check if execution is valid
        solutionCandidateRuntime match {
          case Some(_) => {
            validCandidate = true
            println("candidate has valid runtime, so lets try it")
          }
          case None => println("can't execute current expression")
        }
      }

      println("valid candidate")

      if (solutionRuntime.get > 0) {
        // second chance using probability function
        if (solutionCandidateRuntime.get > solutionRuntime.get) {
          // get random number
          val randomNumber = random.nextInt(100)
          // calculate probability

          println("runtime: " + solutionRuntime)
          println("runtime: " + solutionCandidateRuntime)
          println("T: " + T)
          println("randomNumber: " + randomNumber)
          println("f(s') - f(s): " + math.abs(solutionCandidateRuntime.get - solutionRuntime.get))

          val probability = (T * 100) / math.abs(solutionCandidateRuntime.get - solutionRuntime.get)
          println("probability: " + probability)

          // check number
          if (randomNumber > probability) {
            println("probability said no second chance for: " + solutionCandidateRuntime)
          } else {
            println("probability said yes, second chance: " + solutionCandidateRuntime)
            solution = solutionCandidate
          }
        } else {
          // newer is better
          // use this solution as new one
        }
      }
      // update Temperature T
      T = alpha * T
    }

    // best = null
    val best = null

    ExplorationResult(
      solution,
      best,
      Some(path)
    )
  }

}
