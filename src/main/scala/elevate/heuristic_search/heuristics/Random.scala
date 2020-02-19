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
        if(panel.f(solution) > 0){
          valid = true
        }
      }
    }

    print("solution: \n" + solution)

    val value = panel.f(solution)
    println("f(solution): " + value)
    solution
  }

}
