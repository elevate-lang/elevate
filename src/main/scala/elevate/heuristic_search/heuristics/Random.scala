package elevate.heuristic_search.heuristics

import elevate.heuristic_search.util.Path
import elevate.heuristic_search.{Heuristic, ProblemConstraints}

class Random[P](var solution:P, val panel:ProblemConstraints[P]) extends Heuristic[P] {

  def start(): P = {
    val path = new Path(solution, panel.f(solution).get)
    val N = 10
    val random = scala.util.Random

    for (i <- Range(0, N)) {
      val Ns = panel.N(solution)
      var valid = false
      while(!valid){
        val randomIndex = random.nextInt(Ns.size)
        val result = Ns.toSeq(randomIndex)
        panel.f(result._1) match {
          case Some(value) => {
            valid = true
            solution = result._1
            //add to path
            path.add(solution, result._2, panel.f(solution).get)
          }
          case _ =>
        }
      }
    }

    path.printPathConsole()
    path.writePathToDot()


    solution
  }

}
