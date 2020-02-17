package elevate.heuristic_search.heuristics

import elevate.heuristic_search.{Heuristic, ProblemConstraints}

class Random[P](var solution:P, val panel:ProblemConstraints[P]) extends Heuristic[P] {

  def start(): P = {
    val N = 10
    val random = scala.util.Random

    for (i <- Range(0, N)) {
      val Ns = panel.N(solution)
      val randomIndex = random.nextInt(Ns.size)
      print("i: " + i + " - " + randomIndex + "\n")
      solution = Ns.toSeq(randomIndex)
      print("solution: " + solution + " f(solution): " + panel.f(solution) + "\n")
    }

    solution
  }

}
