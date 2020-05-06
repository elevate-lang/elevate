package elevate.heuristic_search.heuristics

import elevate.heuristic_search.util.Path
import elevate.heuristic_search.{Heuristic, HeuristicPanel}

class Random[P] extends Heuristic[P] {
  // initialize global best
  var best:Option[Double] = None

  def start(panel:HeuristicPanel[P], initialSolution:P, depth: Int): (P, Option[Double], Path[P]) = {
    var solution:P = initialSolution
    val path = new Path(solution, panel.f(solution))
    val random = scala.util.Random

    for (_ <- Range(0, depth)) {
      val Ns = panel.N(solution)
      var valid = false
      var j = 0
      while(!valid && j < Ns.size){
        j = j + 1
        val randomIndex = random.nextInt(Ns.size)
        val result = Ns.toSeq(randomIndex)
        panel.f(result._1) match {
          case Some(value) => {
              valid = true
              solution = result._1
              //add to path
              path.add(solution, result._2, Some(value))

            // check if new global best is found
            best match{
              case None => best = Some(value)
              case Some(_) => {
                value < best.get match {
                  case true => best = Some(value)
                  case false =>
                }
              }
            }

          }
          case _ =>
        }
      }
    }

    (solution, best, path)
  }

}
