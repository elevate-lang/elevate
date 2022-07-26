package elevate.heuristic_search

import java.io.{File, FileOutputStream, PrintWriter}
import elevate.core.Strategy
import elevate.heuristic_search.panel_implementations.{SimpleRewritePanel, StandardPanel}
import elevate.heuristic_search.util.{SearchSpace, Solution}
import jdk.jfr.AnnotationElement

import scala.language.postfixOps
import scala.sys.process._

// runner class
case class Metaheuristic[P](name: String,
                            heuristic: Heuristic[P],
                            depth: Int,
                            samples: Int,
                            repetitions: Int,
                            runner: Runner[P],
                            strategies: Seq[Strategy[P]],
                            output: String,
                            rewriteFunction: Option[Solution[P] => Seq[Solution[P]]],
                            afterRewrite: Option[Strategy[P]],
                            importExport: Option[(String => Solution[P], (Solution[P], String) => Unit)],
                            heuristicPanel: HeuristicPanelChoice = StandardPanelChoice,
                            iteration: Option[Int] = None
                           ) extends Runner[P] {
  var counter = 0

  def plot() = {
    runner.plot()
  }

  //  def execute(solution: P): (P, Option[Double]) = {
  //  def execute(solution: Solution[P]): (P, Option[Double]) = {
  def execute(solution: Solution[P]): ExplorationResult[P] = {


    // new heuristicPanel with runner (is either new metaheuristic or executor)

    val panel = heuristicPanel match {
      case StandardPanelChoice =>
        new StandardPanel[P](
          runner = runner,
          strategies = strategies,
          rewriter = rewriteFunction,
          afterRewrite = afterRewrite,
          importExport = importExport
        )
      case SimpleRewritePanelChoice =>
        new SimpleRewritePanel[P](
          runner = runner,
          strategies = strategies,
          rewriter = rewriteFunction,
          afterRewrite = afterRewrite,
          importExport = importExport
        )
    }


    // conduct heuristic using panel and configs like depth and iterations
    var best: ExplorationResult[P] = ExplorationResult(solution, None, None)
    for (_ <- Range(0, repetitions)) {
      // todo remove this from metaheuristic to exploration (Although generic)
      println("[METAHEURISTIC] : strategy length: " + solution.strategies.size)
      //      val real_output = iteration match {
      //        case None => output
      //        case Some(value) => s"${output}_${value}"
      //      }

      val real_output = output
      s"mkdir -p ${real_output}" !!
      val result = heuristic.start(panel, solution, depth, samples)

      // todo move this to output or is this part of exploration?

      // write strategies to file
      val strategiesString = strategies.mkString("\n")

      val file = new PrintWriter(
        new FileOutputStream(new File(real_output + "/" + "strategies"), false))

      file.write(strategiesString)
      file.close()

      plot()

      //      println("runner is : " + runner.toString)
      val runnerNameHm = runner match {
        case metaheuristic: Metaheuristic[P] => metaheuristic.name + "_0" + "/" + metaheuristic.name + "_hm_0.csv"
        case _ => "Executor/hm/executor_hm.csv"
      }

      // todo copy results from down folder top-folder

      // either copy from executor
      // else copy from heurisitc

      // copy result to top folder
      val command = iteration match {
        case None => s"cp ${real_output}/${runnerNameHm} ${real_output}/${name}_hm.csv"
        case Some(value) => s"cp ${real_output}/${runnerNameHm} ${real_output}/${name}_hm_${value}.csv"
      }


      println("command: " + command)
      command !!

      val runnerName = runner match {
        case metaheuristic: Metaheuristic[P] => metaheuristic.name + "_0" + "/" + metaheuristic.name + "_0.csv"
        case _ => "Executor/executor.csv"
      }

      // copy result to top folder
      val command2 = iteration match {
        case None => s"cp ${real_output}/${runnerName} ${real_output}/${name}.csv"
        case Some(value) => s"cp ${real_output}/${runnerName} ${real_output}/${name}_${value}.csv"
      }

      println("command2: " + command2)
      command2 !!

      // only do this if export is enabled?

      //      // write runtimes
      //      println("[METAHEURISTIC] : write values")
      //            writeValues(output + "/" + name + ".csv", result, name)


      //      // print path
      //      println("[METAHEURISTIC] : write path to dot with size: " + result._3.getSize())
      //      result._3.writeToDot(output + "/" + name + ".dot")

      //      result.searchSpace match {
      //        case Some(value) => value.writeToDot(output + "/" + name + ".dot")
      //        case None => // do nothing
      //      }
      //
      //      try {
      //
      //        result.searchSpace match {
      //          case Some(value) => value.writeSearchSpace(output + "/")
      //          case None => // do nothing
      //        }
      //      } catch {
      //        case e: Throwable => println("could not write search space")
      //      }
      //
      //      result.searchSpace match {
      //        case Some(value) => value.writeToDisk(output)
      //        case None => // do nothing
      //      }


      //      println("[METAHEURISTIC] : collapsed size: " + result._3.getSearchSpace().size)
      //      result.searchSpace.get.writeSearchSpace(output + "/")
      //      println("[METAHEURISTIC] : write path to disk")
      //
      //            result._3.writeSearchSpace(output + "/SearchSpace")
      //                  result._3.writePathToDisk(output)
      //                  result._3.writeToDisk(output)
      //

      (s"mv exploration/opentuner $output" !!)
      (s"mv exploration/random_sampling $output" !!)
      (s"mv exploration/exhaustive $output" !!)

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

