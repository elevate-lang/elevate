package elevate.heuristic_search

import java.io.{File, FileOutputStream, PrintWriter}

import elevate.core.Strategy
import elevate.heuristic_search.heuristics.Random
import elevate.heuristic_search._
import elevate.heuristic_search.util.Path


// runner class
class Metaheuristic[P](val name:String,
                       val heuristic:Heuristic[P],
                       val depth:Int,
                       val iterations: Int,
                       val runner:Runner[P],
                       val strategies:Set[Strategy[P]],
                       val output:String
                      ) extends Runner[P] {

  var counter = 0

  def execute(solution: P): (P, Option[Double]) = {

    // new heuristicPanel with runner (is either new metaheuristic or executor)
    val panel = new HeuristicPanelImplementation[P](runner, strategies)

    // conduct heuristic using panel and configs like depth and iterations
    var best: (P, Option[Double]) = (solution, None)
    for (_ <- Range(0, iterations)) {
      val result = heuristic.start(panel, solution, depth)

      // write runtimes
      writeValues(output + "/" + name + ".csv", result, name)

      // print path
      result._3.writePathToDot(output + "/" + name + ".dot")
      //result._3.writePathToDisk(output + "/" )
      result._3.writePathToDisk(output)

      best._2 match {
        case Some(currentBest) => {
          result._2 match {
            case Some(candidateBest) => {
              // check if new best is found
              if (candidateBest < currentBest) {
                best = (result._1, result._2)
              }
            }
            case _ => // do nothing
          }
        }
          // initialize best
        case None => {
          //just if there is some result
          result._2 match {
            case Some(_) => {
              // initialize best
              best = (result._1, result._2)
            }
            case _ => // do nothing
          }
        }
      }
    }

    best
  }

  def writeValues(path: String, result: (P, Option[Double], Path[P]), name:String) {
    // open file for appendix
    val file = new PrintWriter(new FileOutputStream(new File(path), true))

    // create string to write to file
    var string = counter + ", " + name + ", " + System.currentTimeMillis().toString + ", " + result._1.hashCode().toString + ", "
    result._2 match{
      case Some(value) => string += value.toString + "\n"
      case _ => string += "-1 \n"
    }

    // write to file and close
    file.write(string)
    counter += 1
    file.close()
  }

  def writeHeader(path:String) {
    // open file for appendix
    val file = new PrintWriter(new FileOutputStream(new File(path), true))

    // create string to write to file
    val string = "iteration, " + "runner, " + "timestamp, " + "hash, " + "runtime\n"

    // write to file and close
    file.write(string)
    file.close()
  }

}

