package elevate.heuristic_search.heuristics

import elevate.heuristic_search.{Heuristic, ProblemConstraints}

class Random[P](var solution:P, val panel:ProblemConstraints[P]) extends Heuristic[P] {

  def start(): P = {
    val N = 10
    val random = scala.util.Random

    for (i <- Range(0, N)) {
      val Ns = panel.N(solution)
      var valid = false
      while(!valid){
        val randomIndex = random.nextInt(Ns.size)
        solution = Ns.toSeq(randomIndex)
        panel.f(solution) match {
          case Some(value) => valid = true
          case _ =>
        }
      }
    }

    println("performance: " + panel.f(solution))

    solution
  }

}
