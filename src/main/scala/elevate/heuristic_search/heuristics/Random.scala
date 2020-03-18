package elevate.heuristic_search.heuristics

import elevate.heuristic_search.util.Path
import elevate.heuristic_search.{Heuristic, ProblemConstraints}

class Random[P](var solution:P, val panel:ProblemConstraints[P], val iterations: Int) extends Heuristic[P] {
  // initialize global best
  var best = panel.f(solution)

  def start(): P = {

    val path = new Path(solution, panel.f(solution))

    val random = scala.util.Random


    for (_ <- Range(0, iterations)) {
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

    path.printPathConsole()
    // make path part of settings
    // create folder as well, maybe use relative paths
    path.writePathToDot("/home/jo/developement/rise-lang/exploration/random.dot")


    solution
  }

}
