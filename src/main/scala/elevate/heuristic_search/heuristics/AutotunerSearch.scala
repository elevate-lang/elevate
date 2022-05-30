package elevate.heuristic_search.heuristics

import elevate.heuristic_search.util.{Path, Solution, hashProgram}
import elevate.heuristic_search.{Heuristic, HeuristicPanel}
import jdk.jfr.Timespan

import java.io.{File, FileOutputStream, PrintWriter}
import scala.collection.immutable.Queue
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import java.io.{File, FileOutputStream, PrintWriter}
import scala.collection.mutable.ListBuffer
import scala.language.postfixOps
import scala.sys.process._

class AutotunerSearch[P] extends Heuristic[P] {

  def start(panel:HeuristicPanel[P], initialSolution:Solution[P], depth:Int): (P, Option[Double], Path[P]) = {

    println("depth: " + depth)

    val totalDurationStart = System.currentTimeMillis()

    var solution = initialSolution
    //    var solutionValue = panel.f(solution)
    var solutionValue: Option[Double] = Some(1000.toDouble)

    // create path, queue and hashmap
    val path = new Path(solution.expression, solutionValue, null, null, 0) // still necessary?
    var queue = Queue.empty[(Int, Solution[P])]

    queue = queue.enqueue((0, solution))
    path.hashmap += (hashProgram(solution.expression) -> solution)

    // parallel and synchronized
    def dq():(Int, (Int, Solution[P])) = this.synchronized {

        val current = queue.dequeue

        // update queue
        queue = current._2
        val layer = current._1._1 + 1

      (layer, current._1)
    }

    def enq(layer: Int, Ns: Set[Solution[P]]) = this.synchronized {
      // add elements
      // add elements from neighborhood to queue
      Ns.foreach(ne => {
        // if last layer is reached don't enqueue
        if (layer < depth) {
          queue = queue.enqueue((layer, ne))
        }

        // add to hashmap, if not present yet
        val hash = hashProgram(ne.expression)
        path.hashmap.get(hash) match {
          case Some(_) => // do nothing
          case None => path.hashmap += (hash -> ne)
        }
      })
    }

    // parallel
    def grow(current: (Int, Solution[P])): Set[Solution[P]] = {
      // get neighborhood of current solution
      panel.N(current._2)
    }

    if (depth > 0) {

      var i = 0
      var rewritesTotal = 0
      while (!queue.isEmpty) {
        i += 1

        println("iteration: " + i )
        println("nodes: " + path.hashmap.size)
        println("queue size: " + queue.size)

        // determine thread numbers
        var threads = 1
        if (queue.size >= 24) {
          threads = 1
        }

        val work = queue.size/threads
        println("threads: " + threads)
        println("work per thread: "  + work)
        println("rewrites total: " + rewritesTotal)

        val threadList = mutable.ListBuffer.empty[Thread]

        val layerStart: Long = System.currentTimeMillis()
        var hashMapDuration: Long = 0
        var rewriteDuration: Long = 0
        // maybe use thread pool
        val start = System.currentTimeMillis()
        for (i <- 1 to threads) {
          val thread = new Thread {
            override def run: Unit = {

              // do work on thread
              for (j <- 1 to work) {

                val (layer, current) = this.synchronized {
                  dq()
                }

                val rewriteStart: Long = System.currentTimeMillis()
                val Ns = grow(current)
                rewritesTotal += Ns.size
                rewriteDuration += (System.currentTimeMillis() - rewriteStart)

                val hashMapStart: Long = System.currentTimeMillis()
                this.synchronized {
                  enq(layer, Ns) // add elements synchronized
                }
                hashMapDuration += (System.currentTimeMillis() - hashMapStart)

              }
            }
          }
          thread.start
          threadList.addOne(thread)
        }
        threadList.foreach(thread => thread.join())

        val durationLayer = System.currentTimeMillis() - layerStart
        println("durationTotal: " + durationLayer.toDouble/1000 + " s")
        println("rewriteDuration: " + rewriteDuration.toDouble/1000 + " s")
        println("hashMapDuration: " + hashMapDuration.toDouble/1000 + " s")
        println("\n")
      }
    }

    // convert hashmap to sequence
    val searchSpace = path.hashmap.toSeq.map(elem => elem._2)
    val size = searchSpace.size

    // compare sizes
    println("search space: " + searchSpace.size)
    println("hashmap size: " + path.hashmap.size)

    // measurement point for duration
    val duration:Double = (System.currentTimeMillis() - totalDurationStart).toDouble

    println("duration: " + (duration) + "ms")
    println("duration: " + (duration/1000) + "s")
    println("duration: " +  (duration/1000/60) + "m")

    // start hm with default config file
    println("start random exploration")
    val explorationStartingPoint = System.currentTimeMillis()

    // todo read in these values
//    val doe = size
    val doe = 2
    val optimizationIterations = 10 - doe

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
      "optimization_iterations": ${optimizationIterations},
      "input_parameters" : {
        "i": {
        "parameter_type" : "integer",
        "values" : [0, ${size-1}],
        "constraints" : [],
        "dependencies" : []
      }
      }
    }"""
    }

    // write config String to file
    val file = new File(os.pwd.toString() + "/exploration/configuration/hm_tuning.json")
    new PrintWriter(file) {
      try {
        write(configString)
      } finally {
        close()
      }
    }

    val configFile = os.pwd.toString() + "/exploration/configuration/hm_tuning.json"
    println("configFile: " + configFile)

    // spawn hm process for client-server mode
    val hypermapper = os.proc("hypermapper", configFile).spawn()

    var i = 1
    // main tuning loop
    var done = false
    while (hypermapper.isAlive() && !done) {
      hypermapper.stdout.readLine() match {
        case null =>
          done = true
          println("End of HyperMapper -- error")
        case "End of HyperMapper" =>
          done = true
          println("End of HyperMapper -- done")
        case "Best point found:" =>
          val headers = hypermapper.stdout.readLine()
          val values = hypermapper.stdout.readLine()
          hypermapper.stdout.readLine() // consume empty line
          println(s"Best point found\nHeaders: ${headers}Values: $values")
        case request if request.contains("warning") =>
          println(s"[Hypermapper] $request")
        case request if request.contains("Request") =>
          println(s"Request: $request")
          val numberOfEvalRequests = request.split(" ")(1).toInt
          // read in header
          val header = hypermapper.stdout.readLine().split(",").map(x => x.trim())
          // start forming response
          var response = s"${header.mkString(",")},runtime,Valid\n"
          for (_ <- Range(0, numberOfEvalRequests)) {
            // read in parameters values
            //              val parametersValues = hypermapper.stdout.readLine().split(",").map(x => x.trim())
            val index = hypermapper.stdout.readLine().toInt
            // compute sample (including function value aka runtime)
            print("[" + i.toString + "/" + (doe + optimizationIterations).toString + "] : ")
            print(index.toString + " ")

            val candidate = searchSpace.apply(index)

            // add candidate to visited
            path.visited += (hashProgram(candidate.expression) -> candidate)

            // change this value
            val result = panel.f(candidate)

            // update solution value if better performance is found
            solutionValue = result match {
              case Some(value) => {
                value <= solutionValue.get  match {
                  case true =>
                    //              println("better")
                    // update solution
                    solution = candidate
                    // return result as new solution value
                    result
                  case false =>
                    //              println("not better")
                    solutionValue
                }
              }
              case None => solutionValue
            }

            //            println(sample.runtime)
            println(result)
            println(result)
            println()
            i += 1
            // append sample to Samples

            // append response
            result match {
              case None => response += s"${index},-1,False\n"
              case Some(value) =>
                response += s"${index},${value},True\n"
            }
          }
          print(s"Response: $response")
          // send response to Hypermapper
          hypermapper.stdin.write(response)
          hypermapper.stdin.flush()
        case message => println("message: " + message)
      }
    }

    // todo save output generic, avoid overwriting
    // save output
    ("mkdir -p exploration/tuner" !!)
    ("mv mv_exploration_output_samples.csv " + "exploration/tuner/tuner_exploration.csv" !!)
    (s"mv ${configFile} " + "exploration/tuner/tuner_exploration.json" !!)

    // plot results using hypermapper
    ("hm-plot-optimization-results " +
      "-j " + "exploration/tuner/tuner_exploration.json" + " " +
      "-i " + "exploration/tuner/" + " " +
      "-o" + "exploration/tuner/tuner_exploration.pdf" + " " +
      "--y_label \"Log Runtime(ms)\"" !!)
//      "-log --y_label \"Log Runtime(ms)\"" !!)

    val duration2 = (System.currentTimeMillis() - explorationStartingPoint).toDouble
    println("duration2: " + duration2/1000  + "s")

    println("end")
    println("solution: " + solution.expression)
    println("solutionValue: " + solutionValue)
    println("strategies: ")
    solution.strategies.foreach(println)


    (solution.expression, solutionValue, path)
  }

}
