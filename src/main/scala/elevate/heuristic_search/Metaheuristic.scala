package elevate.heuristic_search

import java.io.{File, FileOutputStream, PrintWriter}
import elevate.core.Strategy
import elevate.heuristic_search.util.{Path, SearchSpace, Solution}
import jdk.jfr.AnnotationElement

import scala.language.postfixOps
import scala.sys.process._

// runner class
case class Metaheuristic[P](name: String,
                            heuristic: Heuristic[P],
                            depth: Int,
                            iterations: Int,
                            runner: Runner[P],
                            strategies: Set[Strategy[P]],
                            output: String,
                            rewriteFunction: Option[Solution[P] => Set[Solution[P]]],
                            afterRewrite: Option[Strategy[P]],
                            importExport: Option[(String => Solution[P], (Solution[P], String) => Unit)]
                           ) extends Runner[P] {
  var counter = 0

  def plot() = {
    runner.plot()
  }

  //  def execute(solution: P): (P, Option[Double]) = {
  //  def execute(solution: Solution[P]): (P, Option[Double]) = {
  def execute(solution: Solution[P]): ExplorationResult[P] = {

    // new heuristicPanel with runner (is either new metaheuristic or executor)
    val panel = new HeuristicPanelImplementation[P](
      runner = runner,
      strategies = strategies,
      rewriter = rewriteFunction,
      afterRewrite = afterRewrite,
      importExport = importExport
    )

    // conduct heuristic using panel and configs like depth and iterations
    var best: ExplorationResult[P] = ExplorationResult(solution, None, None)
    for (_ <- Range(0, iterations)) {
      println("[METAHEURISTIC] : strategy length: " + solution.strategies.size)
      val result = heuristic.start(panel, solution, depth)

      // todo move this to output or is this part of exploration?

      // only do this if export is enabled?

      //      // write runtimes
      //      println("[METAHEURISTIC] : write values")
      //      writeValues(output + "/" + name + ".csv", result, name)
      //
      //
      //      // print path
      //      println("[METAHEURISTIC] : write path to dot with size: " + result._3.getSize())
      //      //      result._3.writeToDot(output + "/" + name + ".dot")
      //      println("[METAHEURISTIC] : collapsed size: " + result._3.getSearchSpace().size)
      //      //      result._3.writePathToDisk(output + "/" )
      //      println("[METAHEURISTIC] : write path to disk")
      //
      //      result._3.writeSearchSpace(output + "/SearchSpace")
      //      //      result._3.writePathToDisk(output)
      //      //      result._3.writeToDisk(output)
      //
      //      plot()

      // move tuner to output
      //      try {
      //        ("mv exploration/tuner/tuner_exploration.csv " + output + "/Executor" !!)
      //        ("mv exploration/tuner/tuner_exploration.pdf " + output + "/Executor" !!)
      //        ("mv exploration/tuner/tuner_exploration.json " + output + "/Executor" !!)
      //      } catch {
      //        case e: Throwable => // ignore
      //      }

      best.performance match {
        case Some(currentBest) =>
          result.performance match {
            case Some(candidateBest) =>
              // check if new best is found
              if (candidateBest < currentBest) {
                best = result
              }
            case _ => // do nothing
          }
        // initialize best
        case None =>
          //just if there is some result
          result.performance match {
            case Some(_) =>
              // initialize best
              best = result
            case _ => // do nothing
          }
      }

    }

    best
  }

  override def checkSolution(solution: Solution[P]): Boolean = {

    true
  }

  def writeValues(path: String,
                  result: (P, Option[Double], SearchSpace[P]),
                  name: String): Unit = {
    // open file for appendix
    val file = new PrintWriter(
      new FileOutputStream(new File(path), true))

    // create string to write to file
    var string = s"$counter, $name, ${System.currentTimeMillis()}, " +
      s"${Integer.toHexString(result._1.hashCode())}, "
    result._2 match {
      case Some(value) => string += value.toString + "\n"
      case _ => string += "-1 \n"
    }

    // write to file and close
    file.write(string)
    counter += 1
    file.close()
  }

  def writeHeader(path: String): Unit = {
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

