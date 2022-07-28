package elevate.exploration

//import elevate.exploration.simpleRewriteExploration.strategies

import elevate.heuristic_search
import elevate.heuristic_search.{ExplorationResult, Runner}
import elevate.heuristic_search.util.{Solution, hashProgram}

import java.io.{File, FileOutputStream, PrintWriter}
import java.nio.file.{Files, Paths}
import java.security.MessageDigest
import scala.collection.mutable.ListBuffer
import scala.language.postfixOps
import scala.sys.process._

// random executor using random number generator and seed
case class RandomExecutor(
                           seeed: Int = 1800,
                           limit: Double = 1000.0,
                           failureRate: Double = 0.2,
                           winningRate: Double = 0.001,
                           iterations: Int = 100,
                           output: String = "exploration"
                         ) extends Runner[SimpleRewrite] {

  val random = new scala.util.Random(seeed)
  val failure = new scala.util.Random(seeed)
  val winner = new scala.util.Random(seeed)
  val tuningResults = new ListBuffer[TuningResultStatistic]()
  var number = 0


  case class TuningResultStatistic(
                                    number: Int,
                                    solution: String,
                                    timestamp: Long,
                                    runtime: Option[Double]
                                  )

  case class Statistics(
                         samples: ListBuffer[TuningResultStatistic]
                       )


  val sha = MessageDigest.getInstance("SHA-1")

  // todo iterate on this method
  // todo think about the search space shape
  override def execute(solution: Solution[SimpleRewrite]): ExplorationResult[SimpleRewrite] = {
    number += 1

    //    println("string: " + solution.expression.mkString(""))
    //    println("size: " + solution.expression.size)

    //    println("size: " + Long.MaxValue.toString.size)


    // hash to 64 bit

    val hash = sha.digest(solution.solutionSteps.last.expression.mkString("").getBytes("UTF-8"))

    import java.math.BigInteger
    val hash2 = new BigInteger(hash).longValue()

    //    println()
    //    println(s"number: [${number}]")
    //    println("solution: " + solution.expression.mkString(""))
    //    println("hash: " + hash)
    //    println("hash2: " + hash2)
    random.setSeed(hash2)

    // set seed based on solution
    //    solution.expression.size match {
    //      case 0 =>
    //        random.setSeed(0)
    //      case _ =>
    //        solution.expression.size % 19 == 0 match {
    //          case true =>
    //            random.setSeed(solution.expression.mkString("").substring(0, (19)).toLong)
    //          case false =>
    //            random.setSeed(solution.expression.mkString("").substring(0, (solution.expression.size % 19)).toLong)
    //        }
    //    }

    val performance = failure.nextInt(100000) > failureRate * 100 * 1000 match {
      case true =>
        Some(random.nextDouble() * 1000)
      case false => None
    }

    //    println(s"[${number}] : " + performance)

    val tresult =
      TuningResultStatistic(
        number = number,
        solution = hashProgram(solution.solutionSteps.last.expression),
        timestamp = System.currentTimeMillis(),
        runtime = performance
      )

    // save
    saveTuningResults(tresult)
    tuningResults += tresult

    ExplorationResult[SimpleRewrite](
      solution,
      performance, // solutions performance
      None
    )
  }

  // check after rewrite is alwasy true
  override def checkSolution(solution: Solution[SimpleRewrite]): Boolean = {

    true
  }

  def saveTuningResults(tuningResultStatistic: TuningResultStatistic) = {

    val filePath = output + "/" + "tuningStatistics.csv"
    val filePathHm = output + "/" + "tuningStatistics_hm.csv"

    val exists = Files.exists(Paths.get(filePath))

    val fWriter = new PrintWriter(new FileOutputStream(new File(filePath), true))
    val fWriterHm = new PrintWriter(new FileOutputStream(new File(filePathHm), true))

    if (!exists) {
      val header = "number, solution, timestamp, runtime" + "\n"
      val headerHm = "index,runtime,Valid,Timestamp" + "\n"

      fWriter.write(header)
      fWriterHm.write(headerHm)
    }

    val (runtime, valid) = tuningResultStatistic.runtime match {
      case Some(value) => (value.toString, "True")
      case None => ("-1", "False")
    }

    // write line
    val line =
      tuningResultStatistic.number.toString + ", " +
        tuningResultStatistic.solution + ", " +
        tuningResultStatistic.timestamp.toString + ", " +
        runtime.toString + "\n"

    // write line hm
    val lineHm =
      tuningResultStatistic.number.toString + "," +
        runtime.toString + "," +
        valid + "," +
        tuningResultStatistic.timestamp.toString + "\n"

    fWriter.write(line)
    fWriterHm.write(lineHm)

    fWriter.close()
    fWriterHm.close()

  }


  override def plot(): Unit = {

    // also write config file

    val doe = tuningResults.size

    val configString = {
      s"""{
      "application_name": "mv_exploration",
      "optimization_objectives": ["runtime"],
      "feasible_output" : {
        "enable_feasible_predictor" : true,
        "name" : "Valid",
        "true_value" : "True",
        "false_value" : "False"
      },
      "hypermapper_mode" : {
        "mode" : "client-server"
      },
      "design_of_experiment": {
        "doe_type": "random sampling",
        "number_of_samples": ${doe}
      },
      "optimization_iterations": 0,
      "input_parameters" : {
        "index": {
        "parameter_type" : "integer",
        "values" : [0, ${doe}],
        "dependencies" : [],
        "constraints" : []
      }
      }
    }"""
    }

    // write configstring
    val configFilePath = output + "/" + "tuningStatistics.json"
    val fWriter = new PrintWriter(new FileOutputStream(new File(configFilePath), true))
    fWriter.write(configString)
    fWriter.close()

    // plot results
    //    val outputFilePath = output + "/" + "tuningStatistics_hm.csv"

    // mkdir output folder
    (s"mkdir -p ${output}/hm " !!)
    (s"mv ${output}/tuningStatistics_hm.csv ${output}/hm" !!)

    // call plot
    val command = s"hm-plot-optimization-results -j ${configFilePath} -i ${output}/hm -l exploration -o ${output}/plot.pdf --y_label 'Runtime(ms)' --plot_log --title exploration"
    println("plot: " + command)
    command !!

  }

}
