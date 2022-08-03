package elevate.heuristic_search.heuristics

import elevate.heuristic_search.util.Solution
import elevate.heuristic_search.{ExplorationResult, Heuristic, HeuristicPanel}

import scala.collection.mutable.Stack

class SimulatedAnnealingPlain[P] extends Heuristic[P] {

  val temperatureRandom = scala.util.Random
  val randomIndex = scala.util.Random

  def start(panel: HeuristicPanel[P], initialSolution: Solution[P], depth: Int, samples: Int): ExplorationResult[P] = {
    var solution = initialSolution

    //    val path = new Path(solution.expression, panel.f(solution), null, null, 0)

    // initialization of helper
    val random = scala.util.Random
    var T = 1000.0
    val alpha = 0.9
    val betterThreshold = 0.9 // think about threshold/margin in which we conside two samples as equal

    // main loop
    var k = 0
    var counter = 0

    println("welcome to simulated annealing")

    //    var oldSolution = initialSolution
    var updated = false
    var Ns = panel.N(solution)

    while (counter < samples) {
      println(s"[${counter + 1}/${samples}] T: ${T}")

      counter += 1
      k += 1

      // get function value (buffered)
      val solutionRuntime = panel.f(solution)

      // update or get neighborhood
      Ns = updated match {
        case true => panel.N(solution) // get fresh neighbors
        case false => Ns // use buffered neighbors
      }

      // get random element from neighborhood
      val solutionCandidate: Solution[P] = Ns.apply(randomIndex.nextInt(Ns.size))

      val fSolutionCandidate: Option[Double] = panel.f(solutionCandidate)

      // update solution based on performance of candidate
      val candidateResult = solutionRuntime match {
        case Some(solutionValue) => fSolutionCandidate match {
          case Some(solutionCandidateValue) =>
            // compare
            solutionCandidateValue < solutionValue match {
              case true =>
                // better
                (solutionCandidate, true)
              case false =>
                // not better, second chance
                secondChance(solutionValue, solutionCandidateValue, T) match {
                  case true => (solutionCandidate, true)
                  case false => (solution, false)
                }
            }
          case None =>
            // not  better but second chance
            secondChance(solutionValue, Int.MaxValue, T) match {
              case true => (solutionCandidate, true)
              case false => (solution, false)
            }
        }
        case None => fSolutionCandidate match {
          case Some(_) => (solutionCandidate, true) // candidate is considered better
          case None =>
            // both None, second chance
            secondChance(Int.MaxValue, Int.MaxValue, T) match {
              case true => (solutionCandidate, true)
              case false => (solution, false)
            }
        }
      }

      // deploy information
      solution = candidateResult._1
      updated = candidateResult._2

      // update temperature
      T = alpha * T
    }

    val best = null

    ExplorationResult(
      solution,
      best,
      None
    )
  }

  def secondChance(
                    solutionRuntime: Double,
                    solutionCandidateRuntime: Double,
                    temperature: Double
                  ): Boolean = {

    // calculate probability

    // check if we have a value


    //          println("runtime: " + solutionRuntime)
    //          println("runtime: " + solutionCandidateRuntime)
    //          println("T: " + T)
    //          println("randomNumber: " + randomNumber)
    //          println("f(s') - f(s): " + math.abs(solutionCandidateRuntime.get - solutionRuntime.get))

    // get random number
    val randomNumber = temperatureRandom.nextInt(100)
    val probability = (temperature * 100) / math.abs(solutionCandidateRuntime - solutionRuntime)

    // decide
    randomNumber > probability
  }

}
