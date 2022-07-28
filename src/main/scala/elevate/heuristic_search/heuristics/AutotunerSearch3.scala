//package elevate.heuristic_search.heuristics
//
//import elevate.heuristic_search.util._
//import elevate.heuristic_search._
//
//import scala.collection.immutable.Queue
//import scala.collection.mutable
//import scala.language.postfixOps
//import scala.sys.process._
//
//class AutotunerSearch3[P] extends Heuristic[P] {
//  var layerPrint = 0
//  var counterTotal = 0
//  val rewriteLimit = 6
//  var globalLeaves = mutable.Set.empty[String]
//  var durationRewriting: Long = 0
//  var durationGetSolution: Long = 0
//
//  def start(panel: HeuristicPanel[P], initialSolution: Solution[P], depth: Int, samples: Int): ExplorationResult[P] = {
//
//    val dry = false
//    val generate = true
//
//    val (tree, filepath) = generate match {
//      case true =>
//        // generate search space
//        //        val tree = generateSearchSpace(panel, initialSolution, depth)
//        val tree = createTree(initialSolution, panel)
//
//        //        val constraintsInverted = tree.getConstraintsInvert()
//        //        System.exit(0)
//        //        val filepath2 = tree.toJsonNumbers3("exploration/tree.json")
//
//        // export tree
//        val filepath = tree.toJsonNumbers("exploration/tree.json")
//
//
//        (tree, filepath)
//      case false =>
//        // read in config file
//        val filepath = "exploration/tree_5_big.json"
//
//        // create empty tree with only one element
//        val tree = new SimpleTree[P](
//          initial = new SimpleTreeElement[P](
//            solutionHash = hashProgram(initialSolution),
//            rewrite = -1, // edge case
//            layer = 0,
//            predecessor = null,
//            successor = Set.empty[SimpleTreeElement[P]]
//          ),
//          initialSolution = initialSolution
//        )
//        (tree, filepath)
//    }
//
//
//    dry match {
//      case true =>
//        ExplorationResult(
//          initialSolution,
//          None,
//          Some(tree)
//        )
//      case false =>
//        //     explore search space
//        val (solution, solutionValue) = explore(panel, initialSolution, depth, filepath)
//        ExplorationResult(
//          solution,
//          solutionValue,
//          Some(tree)
//        )
//    }
//
//  }
//
//  // breadth first
//  def createTree(initialSolution: Solution[P], panel: HeuristicPanel[P]): SimpleTree[P] = {
//
//    // create empty tree
//    val tree = new SimpleTree[P](
//      initial = new SimpleTreeElement[P](
//        solutionHash = hashProgram(initialSolution.expression),
//        rewrite = -1,
//        layer = 0,
//        predecessor = null,
//        successor = Set.empty[SimpleTreeElement[P]]
//      ),
//      initialSolution = initialSolution
//    )
//
//    // create queue
//    var queue = Queue.empty[SimpleTreeElement[P]]
//
//    // empty tree
//    // initial tree element
//    queue = queue.enqueue(tree.initial)
//
//    println("queue: " + queue.size)
//    println("queue.empty: " + !queue.isEmpty)
//
//    var layer = 0
//    while (!queue.isEmpty) {
//      val layerDurationStart = System.currentTimeMillis()
//      println("\nlayer: " + layer)
//      println("queue.size: " + queue.size)
//
//      // dequeue complete layer
//      var layerElements = 0
//      val layerNodes = queue.size
//      for (i <- 1 to queue.size) {
//
//        // dequeue element
//        val (currentElement, currentQueue) = queue.dequeue
//        queue = currentQueue
//
//        val rewriteNumbers = tree.getRewriteNumbers(currentElement)
//        val solution = panel.getSolution(tree.initialSolution, rewriteNumbers)
//
//        val Ns = panel.N(solution.get)
//        layerElements += Ns.size
//
//        // rewrite
//        Ns.foreach(ne => {
//
//          val hash = hashProgram(ne.expression)
//
//          // add new element if condition is met
//          val cond = globalLeaves.contains(hash)
//          if (!cond) {
//
//            // create new tree element
//            val currentTreeElement = new SimpleTreeElement[P](
//              solutionHash = hash,
//              rewrite = SearchSpaceHelper.strategies.apply(ne.strategies.last.toString()),
//              layer = currentElement.layer + 1,
//              predecessor = currentElement,
//              successor = Set.empty[SimpleTreeElement[P]]
//            )
//
//            // add element to predecessor
//            currentElement.successor += currentTreeElement
//
//            globalLeaves.addOne(hash)
//
//            // add to queue
//            if (layer < rewriteLimit) {
//              queue = queue.enqueue(currentTreeElement)
//            }
//          }
//        })
//
//        // add one for id element
//        // do we need to count this? It is technically not rewritten, but added to next layer
//        layerElements += 1
//
//        // add id element manually
//        val dummyElement = new SimpleTreeElement[P](
//          solutionHash = currentElement.solutionHash,
//          rewrite = 0,
//          layer = currentElement.layer + 1,
//          predecessor = currentElement,
//          successor = Set.empty[SimpleTreeElement[P]]
//        )
//
//        // add element to predecessor
//        currentElement.successor += dummyElement
//
//        // add to queue if limit is not reached
//        if (layer < rewriteLimit) {
//          queue = queue.enqueue(dummyElement)
//        } else {
//
//        }
//      }
//
//      // todo collect intermediate jsons at output folder
//      // write json
//      //      val test = tree.toJsonNumbers2("exploration/tree_" + layer.toString + ".json")
//      val test = tree.toJsonNumbers("exploration/tree_" + layer.toString + ".json")
//      println("write json: " + test)
//      layer += 1
//
//      // todo write this to file
//      // layer end
//      val layerDuration = System.currentTimeMillis() - layerDurationStart
//      println("duration: " + layerDuration.toDouble / 1000 + " s")
//      println("duration: " + layerDuration.toDouble / 1000 / 60 + " m")
//      println("elements: " + layerElements)
//      println("durationPerElement: " + layerDuration / layerElements + " ms")
//      println("avg children per node: " + layerElements.toDouble / layerNodes.toDouble)
//    }
//
//    tree
//  }
//
//  var counter = 0
//
//  // depth first
//  def rewriteNode(panel: HeuristicPanel[P], simpleTreeElement: SimpleTreeElement[P], simpleTree: SimpleTree[P]): Unit = {
//
//    if (counter % 1000 == 0) {
//      println("treeSize: " + simpleTree.getSize())
//    }
//    counter += 1
//
//    //    simpleTree.printConsole()
//
//    if (simpleTreeElement.layer <= rewriteLimit) {
//
//
//      // reproduce element from numbers
//
//      val getSolutionStart = System.currentTimeMillis()
//      // get solution from current SimpleTreeElement
//      val rewriteNumbers = simpleTree.getRewriteNumbers(simpleTreeElement)
//      //      val solution = computeSample2(panel, simpleTree.initialSolution, rewriteNumbers)
//      val solution = panel.getSolution(simpleTree.initialSolution, rewriteNumbers)
//      durationGetSolution += (System.currentTimeMillis() - getSolutionStart)
//
//      val rewritesStart = System.currentTimeMillis()
//      val Ns = panel.N(solution.get)
//      durationRewriting += (System.currentTimeMillis() - rewritesStart)
//
//      //      solution.strategies.size match {
//      //        case 0 => println("solution: " + "initial - " + Ns.size)
//      //        case _ => println("solution: " + solution.strategies.last.toString() + " - " + Ns.size)
//      //      }
//      //
//      //      println("globalLeafs: " + globalLeaves.size)
//
//      Ns.foreach(ne => {
//        // check if we have this node already
//        val cond = globalLeaves.contains(hashProgram(ne.expression))
//
//        if (cond) {
//          //          println("already in: " + hashProgram(ne.expression) + " - " + ne.strategies.last.toString())
//          // don't add
//        } else {
//          // add
//          //          println("add: " + hashProgram(ne.expression) + " - " + ne.strategies.last.toString())
//
//          // create new tree element
//          val neTree = new SimpleTreeElement[P](
//            solutionHash = hashProgram(ne.expression),
//            rewrite = SearchSpaceHelper.strategies.apply(ne.strategies.last.toString()),
//            layer = simpleTreeElement.layer + 1,
//            predecessor = simpleTreeElement,
//            successor = Set.empty[SimpleTreeElement[P]]
//          )
//
//          // add element to predecessor
//          simpleTreeElement.successor += neTree
//
//          globalLeaves.addOne(hashProgram(ne.expression))
//
//        }
//      })
//
//      // add id element manually
//      val dummyElement = new SimpleTreeElement[P](
//        solutionHash = simpleTreeElement.solutionHash,
//        rewrite = 0,
//        layer = simpleTreeElement.layer + 1,
//        predecessor = simpleTreeElement,
//        successor = Set.empty[SimpleTreeElement[P]]
//      )
//
//      // add element to predecessor
//      simpleTreeElement.successor += dummyElement
//
//      //      println("globalLeafs: " + globalLeaves.size)
//      //      println("tree size: " + simpleTree.countNodes(simpleTree.initial))
//      //      println("tree leafs: " + simpleTree.getSize())
//      //      println("tree leafs: " + simpleTree.countLeafs(simpleTree.initial))
//
//      simpleTreeElement.successor.foreach(succ => {
//        // call rewrite node
//        if (succ.layer <= rewriteLimit) {
//          rewriteNode(panel, succ, simpleTree)
//        }
//      })
//    }
//  }
//
//  def generateSearchSpace(panel: HeuristicPanel[P], initialSolution: Solution[P], depth: Int): SimpleTree[P] = {
//
//    // create simple tree with initial element
//    val tree = new SimpleTree[P](
//      initial = new SimpleTreeElement[P](
//        solutionHash = hashProgram(initialSolution.expression),
//        rewrite = -1,
//        layer = 0,
//        predecessor = null,
//        successor = Set.empty[SimpleTreeElement[P]]
//      ),
//      initialSolution = initialSolution
//    )
//
//    println("create tree")
//    // create tree
//    val upperBound = math.pow((SearchSpaceHelper.strategies.size + 1) / 2, rewriteLimit).toLong
//    println("upper bound: " + upperBound)
//
//    val startTime = System.currentTimeMillis()
//
//    globalLeaves.addOne(hashProgram(initialSolution.expression))
//
//    rewriteNode(panel, tree.initial, tree)
//
//    val duration = System.currentTimeMillis() - startTime
//
//    println("\ndurations: ")
//    println("duration total: " + duration.toDouble / 1000 + " s")
//    println("duration total: " + duration.toDouble / 1000 / 60 + " m")
//
//    println("getSolution overhead: " + durationGetSolution.toDouble / 1000 + " s")
//    println("getSolution overhead: " + durationGetSolution.toDouble / 1000 / 60 + " m")
//    println("getSolution percentage: " + durationGetSolution.toDouble / duration.toDouble)
//
//    println("rewrite overhead: " + durationRewriting.toDouble / 1000 + " s")
//    println("rewrite overhead: " + durationRewriting.toDouble / 1000 / 60 + " m")
//    println("rewrite percentage: " + durationRewriting.toDouble / duration.toDouble)
//
//
//    println("\nsizes: ")
//    println("tree size: " + tree.getSizeTotal())
//    println("tree leaves: " + tree.getSize())
//
//    println("\ngenerate constraints")
//    val constraints = tree.getConstraints()
//
//    var sum = 0
//    constraints.foreach(elem => {
//      sum += elem._2.size
//    })
//
//    println("size: " + sum)
//
//    tree
//  }
//
//  def explore(panel: HeuristicPanel[P], initialSolution: Solution[P], depth: Int, filePath: String): (Solution[P], Option[Double]) = {
//
//    case class Sample(
//                       solution: Solution[P],
//                       runtime: Option[Double]
//                     )
//
//    def computeSample(header: Array[String], parametersValues: Array[String]): Sample = {
//
//      // reproduce expression based on rewrites
//      val parametersValuesMap = header.zip(parametersValues).map(elem => (elem._1.substring(1).toFloat.toInt, elem._2.toFloat.toInt))
//
//      val strategies = SearchSpaceHelper.getStrategies(parametersValuesMap.toSeq.sortBy(x => x._1).map(x => x._2))
//
//      val rewriteNumbers = parametersValuesMap.toSeq.sortBy(x => x._1).map(x => x._2)
//
//      val solution = panel.getSolution(initialSolution, rewriteNumbers)
//
//      solution match {
//        case Some(value) => {
//
//          val runtime = panel.f(solution.get)
//
//          Sample(solution.get, runtime)
//        }
//        // todo fix problem with initial solution
//        case None => {
//          println(rewriteNumbers.mkString("[", ",", "]"))
//          println(strategies.mkString("[", ", ", "]"))
//          //          throw new Exception("cannot reproduce expression")
//          Sample(initialSolution, None)
//        }
//      }
//    }
//
//    val totalDurationStart = System.currentTimeMillis()
//
//    var solutionValue: Option[Double] = None
//    var solution = initialSolution
//
//    // warning: just for printing, information is not used
//    val doe = 100
//    val optimizationIterations = 100
//
//    val configFile = os.pwd.toString() + "/" + filePath
//    println("configFile: " + configFile)
//
//    // spawn hm process for client-server mode
//    val hypermapper = os.proc("hypermapper", configFile).spawn()
//
//    var i = 1
//    // main tuning loop
//    var done = false
//    while (hypermapper.isAlive() && !done) {
//      hypermapper.stdout.readLine() match {
//        case null =>
//          done = true
//          println("End of HyperMapper -- error")
//        case "End of HyperMapper" =>
//          done = true
//          println("End of HyperMapper -- done")
//        case "Best point found:" =>
//          val headers = hypermapper.stdout.readLine()
//          val values = hypermapper.stdout.readLine()
//          hypermapper.stdout.readLine() // consume empty line
//          println(s"Best point found\nHeaders: ${headers}Values: $values")
//        case request if request.contains("warning") =>
//          println(s"[Hypermapper] $request")
//        case request if request.contains("Request") =>
//          print("[" + i.toString + "/" + (doe + optimizationIterations).toString + "] : ")
//          println(s"$request")
//          val numberOfEvalRequests = request.split(" ")(1).toInt
//          // read in header
//          val header = hypermapper.stdout.readLine().split(",").map(x => x.trim())
//          // start forming response
//          var response = s"${header.mkString(",")},runtime,Valid\n"
//          for (j <- Range(0, numberOfEvalRequests)) {
//            // read in parameters values
//            val parametersValues = hypermapper.stdout.readLine().split(",").map(x => x.trim())
//            // compute sample (including function value aka runtime)
//            //            print("[" + i.toString + "/" + (doe + optimizationIterations).toString + "] : ")
//
//            val sample = computeSample(header, parametersValues)
//
//            solutionValue = solutionValue match {
//              case Some(solutionValue) => {
//                sample.runtime match {
//                  case Some(runtime) => {
//                    runtime <= solutionValue match {
//                      case true => {
//                        solution = sample.solution
//                        Some(runtime)
//                      }
//                      case false => Some(solutionValue)
//                    }
//                  }
//                  case None => Some(solutionValue)
//                }
//              }
//
//              // if not initialized do so
//              case None => sample.runtime match {
//                case Some(value) => {
//                  solution = sample.solution
//                  Some(value)
//                }
//                case None => None
//              }
//            }
//
//            println(j.toString + ": " + sample.runtime)
//            //            println(sample)
//            i += 1
//            // append sample to Samples
//            //                        samples += sample
//            // append response
//            sample.runtime match {
//              case None =>
//                // runtime int Max
//                val runtime: String = "2147483647"
//
//                response += s"${parametersValues.map(x => x.toFloat.toInt).mkString(",")},${runtime},False\n"
//              case Some(value) =>
//                // make sure to response int values
//                response += s"${parametersValues.map(x => x.toFloat.toInt).mkString(",")},${value},True\n"
//            }
//          }
//
//
//          print(s"Response:\n$response")
//          println()
//          // send response to Hypermapper
//          hypermapper.stdin.write(response)
//          hypermapper.stdin.flush()
//        case message => println("message: " + message)
//      }
//    }
//
//    // apply this at the end
//    // save output
//    println("save output")
//    ("mkdir -p exploration/tuner" !!)
//    ("mv mv_exploration_output_samples.csv " + "exploration/tuner/tuner_exploration.csv" !!)
//    (s"cp ${configFile} " + "exploration/tuner/tuner_exploration.json" !!)
//
//    println("plot results")
//    // plot results using hypermapper
//    val test = ("hm-plot-optimization-results " +
//      "-j " + "exploration/tuner/tuner_exploration.json" + " " +
//      "-i " + "exploration/tuner/" + " " +
//      "-o" + "exploration/tuner/tuner_exploration.pdf" + " " +
//      "--y_label \"Log Runtime(ms)\"" !!)
//    //          "-log --y_label \"Log Runtime(ms)\"" !!)
//
//    println("output: " + test)
//
//
//    val duration = (System.currentTimeMillis() - totalDurationStart).toDouble
//
//    println("duration2: " + duration / 1000 + " s")
//    println("duration2: " + duration / 1000 / 60 + " m")
//
//    (solution, solutionValue)
//  }
//
//}
//
