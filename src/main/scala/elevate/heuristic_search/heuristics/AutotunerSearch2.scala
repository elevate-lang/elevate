package elevate.heuristic_search.heuristics

import elevate.heuristic_search.util.{SearchSpaceHelper, Solution, Tree, TreeElement, hashProgram}
import elevate.heuristic_search._

import scala.collection.mutable
import scala.language.postfixOps
import scala.sys.process._

class AutotunerSearch2[P] extends Heuristic[P] {
  var layerPrint = 0
  var counterTotal = 0
  val rewriteLimit = 3
  var globalLeaves = mutable.Set.empty[String]
  var durationRewriting: Long = 0

  def start(panel: HeuristicPanel[P], initialSolution: Solution[P], depth: Int, samples: Int): ExplorationResult[P] = {
    // we don't need this here
    //    val path = new Path(initialSolution.expression, null, null, null, 0) // still necessary?

    val dry = true
    val generate = true

    val (tree, filepath) = generate match {
      case true =>
        // generate search space
        val tree = generateSearchSpace(panel, initialSolution, depth)

        // export tree
        val filepath = tree.toJsonNumbers("exploration/tree.json")

        (tree, filepath)
      case false =>
        // read in config file
        val filepath = "exploration/tree_8.json"

        // create empty tree with only one element
        val tree = new Tree[P](
          initial = new TreeElement[P](
            solution = initialSolution,
            strategy = null,
            layer = 0,
            value = None,
            predecessor = null,
            successor = Set.empty[TreeElement[P]]
          )
        )
        (tree, filepath)
    }


    dry match {
      case true =>
        ExplorationResult(
          initialSolution, None, Some(tree)
        )
      case false =>
        //     explore search sapce
        val (solution, solutionValue) = explore(panel, initialSolution, depth, filepath)
        ExplorationResult(
          solution, solutionValue, Some(tree)
        )
    }

  }

  var counter = 0

  // depth first
  def rewriteNode(panel: HeuristicPanel[P], treeElement: TreeElement[P], tree: Tree[P]): Unit = {

    if (counter % 1000 == 0) {
      println("treeSize: " + tree.getSize())
    }
    counter += 1

    if (treeElement.layer < rewriteLimit) {

      val rewritesStart = System.currentTimeMillis()
      val Ns = panel.N(treeElement.solution)
      durationRewriting += (System.currentTimeMillis() - rewritesStart)

      Ns.foreach(ne => {
        // check if we have this node already
        val cond = globalLeaves.contains(hashProgram(ne.expression))

        if (cond) {
          // don't add
        } else {
          // add

          // create new tree element
          val neTree = new TreeElement[P](
            strategy = ne.strategies.last,
            solution = ne, // maybe not used
            layer = treeElement.layer + 1,
            value = None, // maybe not used
            predecessor = treeElement,
            successor = Set.empty[TreeElement[P]]
          )

          // add element to predecessor
          treeElement.successor += neTree

          globalLeaves.addOne(hashProgram(ne.expression))

        }
      })

      // add id element manually
      val dummyElement = new TreeElement[P](
        strategy = elevate.core.strategies.basic.id[P],
        solution = new Solution[P](treeElement.solution.expression, treeElement.solution.strategies :+ elevate.core.strategies.basic.id[P]),
        //        solution = treeElement.solution,
        layer = treeElement.layer + 1,
        value = None, // maybe not used
        predecessor = treeElement,
        successor = Set.empty[TreeElement[P]]
      )

      // add element to predecessor
      treeElement.successor += dummyElement

      treeElement.successor.foreach(succ => {
        // call rewrite node
        if (succ.layer < rewriteLimit) {
          rewriteNode(panel, succ, tree)
        }
      })
    }
  }

  def generateSearchSpace(panel: HeuristicPanel[P], initialSolution: Solution[P], depth: Int): Tree[P] = {

    // create tree with initial element
    val tree = new Tree[P](
      initial = new TreeElement[P](
        solution = initialSolution,
        strategy = null,
        layer = 0,
        value = None,
        predecessor = null,
        successor = Set.empty[TreeElement[P]]
      )
    )

    println("create tree")
    // create tree

    val startTime = System.currentTimeMillis()

    rewriteNode(panel, tree.initial, tree)

    val duration = System.currentTimeMillis() - startTime

    println("\ndurations: ")
    println("duration total: " + duration.toDouble / 1000 + " s")
    println("duration total: " + duration.toDouble / 1000 / 60 + " m")
    println("rewrite overhead: " + durationRewriting.toDouble / 1000 + " s")
    println("rewrite overhead: " + durationRewriting.toDouble / 1000 / 60 + " m")
    println("rewrite percentage: " + durationRewriting.toDouble / duration.toDouble)

    println("\nsizes: ")
    println("tree size: " + tree.getSizeTotal())
    println("tree leaves: " + tree.getSize())

    println("\ngenerate constraints")
    val constraints = tree.getConstraints()
    //    constraints.foreach(constr => {
    //      println("layer: " + constr._1)
    //      constr._2.foreach(elem => {
    //        println(elem)
    //      })
    //      println("\n")
    //
    //    })

    var sum = 0
    constraints.foreach(elem => {
      sum += elem._2.size
    })

    println("size: " + sum)

    tree
  }

  def explore(panel: HeuristicPanel[P], initialSolution: Solution[P], depth: Int, filePath: String): (Solution[P], Option[Double]) = {

    case class Sample(
                       solution: Solution[P],
                       runtime: Option[Double]
                     )

    def computeSample(header: Array[String], parametersValues: Array[String]): Sample = {

      // reproduce expression based on rewrites
      val parametersValuesMap = header.zip(parametersValues).map(elem => (elem._1.substring(1).toFloat.toInt, elem._2.toFloat.toInt))

      val strategies = SearchSpaceHelper.getStrategies(parametersValuesMap.toSeq.sortBy(x => x._1).map(x => x._2))

      var solution = initialSolution
      strategies.foreach(strategy => {

        // get neighbourhood
        val Ns = panel.N(solution)
        Ns.foreach(ns => {

          if (ns.strategies.last.toString().equals(strategy)) {
            // apply!
            solution = ns
          }
          // check if this is your number
        })
      })

      val runtime = panel.f(solution)

      Sample(solution, runtime)
    }

    val totalDurationStart = System.currentTimeMillis()

    var solutionValue: Option[Double] = None
    var solution = initialSolution

    // warning: just for printing, information is not used
    val doe = 100
    val optimizationIterations = 100

    //    val configFile = os.pwd.toString() + "/exploration/tree.json"
    //    val configFile = os.pwd.toString() + "/exploration/tree_8.json"
    val configFile = os.pwd.toString() + "/" + filePath
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
          print("[" + i.toString + "/" + (doe + optimizationIterations).toString + "] : ")
          println(s"$request")
          val numberOfEvalRequests = request.split(" ")(1).toInt
          // read in header
          val header = hypermapper.stdout.readLine().split(",").map(x => x.trim())
          // start forming response
          var response = s"${header.mkString(",")},runtime,Valid\n"
          for (j <- Range(0, numberOfEvalRequests)) {
            // read in parameters values
            val parametersValues = hypermapper.stdout.readLine().split(",").map(x => x.trim())
            // compute sample (including function value aka runtime)
            //            print("[" + i.toString + "/" + (doe + optimizationIterations).toString + "] : ")

            val sample = computeSample(header, parametersValues)

            solutionValue = solutionValue match {
              case Some(solutionValue) => {
                sample.runtime match {
                  case Some(runtime) => {
                    runtime <= solutionValue match {
                      case true => {
                        solution = sample.solution
                        Some(runtime)
                      }
                      case false => Some(solutionValue)
                    }
                  }
                  case None => Some(solutionValue)
                }
              }

              // if not initialized do so
              case None => sample.runtime match {
                case Some(value) => {
                  solution = sample.solution
                  Some(value)
                }
                case None => None
              }
            }

            println(j.toString + ": " + sample.runtime)
            //            println(sample)
            i += 1
            // append sample to Samples
            //            samples += sample
            // append response
            sample.runtime match {
              case None =>
                // runtime int Max
                val runtime: String = "2147483647"

                response += s"${parametersValues.map(x => x.toFloat.toInt).mkString(",")},${runtime},False\n"
              case Some(value) =>
                // make sure to response int values
                response += s"${parametersValues.map(x => x.toFloat.toInt).mkString(",")},${value},True\n"
            }
          }


          print(s"Response:\n$response")
          println()
          // send response to Hypermapper
          hypermapper.stdin.write(response)
          hypermapper.stdin.flush()
        case message => println("message: " + message)
      }
    }
    //
    //    // todo save output generic, avoid overwriting
    // apply this at the end
    // save output
    println("save output")
    ("mkdir -p exploration/tuner" !!)
    ("mv mv_exploration_output_samples.csv " + "exploration/tuner/tuner_exploration.csv" !!)
    (s"cp ${configFile} " + "exploration/tuner/tuner_exploration.json" !!)

    println("plot results")
    // plot results using hypermapper
    val test = ("hm-plot-optimization-results " +
      "-j " + "exploration/tuner/tuner_exploration.json" + " " +
      "-i " + "exploration/tuner/" + " " +
      "-o" + "exploration/tuner/tuner_exploration.pdf" + " " +
      "--y_label \"Log Runtime(ms)\"" !!)
    //          "-log --y_label \"Log Runtime(ms)\"" !!)

    println("output: " + test)

    //        val duration2 = (System.currentTimeMillis() - explorationStartingPoint).toDouble
    //        println("duration2: " + duration2/1000  + "s")
    //
    //    println("end")
    //    println("solution: " + solution.expression)
    //    println("solutionValue: " + solutionValue)
    //    println("strategies: ")
    //    solution.strategies.foreach(println)
    //

    (solution, solutionValue)
  }

}

