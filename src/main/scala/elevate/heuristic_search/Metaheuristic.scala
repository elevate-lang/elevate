package elevate.heuristic_search

import java.io.{File, FileOutputStream, PrintWriter}

import elevate.core.Strategy
import elevate.heuristic_search.util.{Path, Solution}


// runner class
case class Metaheuristic[P](name: String,
                            heuristic: Heuristic[P],
                            depth: Int,
                            iterations: Int,
                            runner: Runner[P],
                            strategies: Set[Strategy[P]],
                            output: String
                      ) extends Runner[P] {
  var counter = 0

//  def execute(solution: P): (P, Option[Double]) = {
    def execute(solution: Solution[P]): (P, Option[Double]) = {

    // new heuristicPanel with runner (is either new metaheuristic or executor)
    val panel = new HeuristicPanelImplementation[P](runner, strategies)

    // conduct heuristic using panel and configs like depth and iterations
    var best: (P, Option[Double]) = (solution.expression, None)
    for (_ <- Range(0, iterations)) {
      println("[METAHEURISTIC] : strategy length: " + solution.strategies.size)
      val result = heuristic.start(panel, solution, depth)

      // write runtimes
      println("[METAHEURISTIC] : write values")
      writeValues(output + "/" + name + ".csv", result, name)


      // print path
      println("[METAHEURISTIC] : write path to dot with size: " + result._3.getSize())
      println("[METAHEURISTIC] : collapsed size: " + result._3.getSearchSpace().size)
      result._3.writePathToDot(output + "/" + name + ".dot")
//      result._3.writePathToDisk(output + "/" )
      println("[METAHEURISTIC] : write path to disk")
//      result._3.writePathToDisk(output)

      best._2 match {
        case Some(currentBest) =>
          result._2 match {
            case Some(candidateBest) =>
              // check if new best is found
              if (candidateBest < currentBest) {
                best = (result._1, result._2)
              }
            case _ => // do nothing
          }
        // initialize best
        case None =>
          //just if there is some result
          result._2 match {
            case Some(_) =>
              // initialize best
              best = (result._1, result._2)
            case _ => // do nothing
          }
      }
    }

    best
  }

  def writeValues(path: String,
                  result: (P, Option[Double], Path[P]),
                  name: String): Unit = {
    // open file for appendix
    val file = new PrintWriter(
      new FileOutputStream(new File(path), true))

    // create string to write to file
    var string =s"$counter, $name, ${System.currentTimeMillis()}, " +
      s"${Integer.toHexString(result._1.hashCode())}, "
    result._2 match{
      case Some(value) => string += value.toString + "\n"
      case _ => string += "-1 \n"
    }

    // write to file and close
    file.write(string)
    counter += 1
    file.close()
  }

  def writeHeader(path:String): Unit = {
    // open file for appendix
    val file = new PrintWriter(
      new FileOutputStream(new File(path), true))

    // create string to write to file
    val string = "iteration, runner, timestamp, hash, runtime\n"

    // write to file and close
    file.write(string)
    file.close()
  }

}

