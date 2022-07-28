// ignore this for now

//package elevate.heuristic_search.util
//
//import elevate.core.Strategy
//
//import java.io.{File, FileOutputStream, PrintWriter}
//import scala.collection.mutable
//import scala.collection.mutable.ListBuffer
//import scala.language.postfixOps
//import scala.sys.process._
//
//class TreeElement[P](override val solution: Solution[P], // do we need this here?
//                     override val value: Option[Double], // later
//                     val strategy: Strategy[P], // node itself
//                     val layer: Int,
//                     val predecessor: TreeElement[P],
//                     var successor: Set[TreeElement[P]], // mutable ? -> tree in scala?
//                    ) extends SearchSpaceElement[P](solution, value) {
//}
//
//
//class Tree[P](val initial: TreeElement[P]) extends SearchSpace[P] {
//
//  // we don't need the add here
//  override def add(solution: Solution[P], value: Option[Double]): Unit = ???
//
//  override def printConsole(): Unit = {
//    printNode(initial)
//  }
//
//  override def getSizeTotal(): Int = {
//    countNodes(initial)
//  }
//
//  override def getSize(): Int = {
//    val countStart = System.currentTimeMillis()
//    val result = countLeafs(initial)
//    val duration = System.currentTimeMillis() - countStart
//    print("getSize: " + duration.toDouble / 1000 + "s - ")
//
//
//    result
//  }
//
//
//  override def getSearchSpace(): Seq[Solution[P]] = {
//    leafs().map(x => x.solution)
//  }
//
//  // warning! can be slow
//  override def getElement(i: Int): SearchSpaceElement[P] = {
//    leafs().apply(i)
//  }
//
//  override def writeToDot(filename: String): String = {
//    // write string to file
//    val uniqueFilename_full = SearchSpaceHelper.getUniqueFilename(filename, 4)
//    val pw = new PrintWriter(new FileOutputStream(new File(uniqueFilename_full), false))
//
//    val begin = "strict digraph path {\n"
//    pw.write(begin)
//    println("writen nodes")
//    writeNodesDot(initial, pw)
//    println("write edges")
//    writeEdgesDot(initial, pw)
//    val end = "}"
//    pw.write(end)
//    pw.close()
//
//    // visualize dot graph
//    (s"dot -Tpng -O " + uniqueFilename_full !!)
//    (s"dot -Tpdf -O " + uniqueFilename_full !!)
//
//    ""
//  }
//
//  override def writeToDisk(filename: String): Unit = {
//
//    // save expressions
//    // todo write expression to disk
//    writeExpressionsDisk(filename)
//
//    // save json for export
//    //    toJsonNumbers(filename)
//    // todo make this more generic, currently json is written and copied elsewhere
//
//  }
//
//  override def writeSearchSpace(filename: String): Unit = ???
//
//
//  // helper
//
//  // export
//  def printNode(treeElement: TreeElement[P]): Unit = {
//
//    println("treeElement: " + treeElement.strategy)
//    println("layer: " + treeElement.layer)
//    if (treeElement.predecessor == null) {
//      println("predecessor: " + "null")
//    } else {
//      println("predecessor: " + treeElement.predecessor.strategy)
//    }
//    println("successor: " + treeElement.successor.toSeq.map(elem => elem.strategy).mkString("[", ", ", "]"))
//    println("\n")
//
//    treeElement.successor.foreach(succ => {
//      printNode(succ)
//    })
//  }
//
//  def writeNodesDot(treeElement: TreeElement[P], pw: PrintWriter): Unit = {
//
//
//    // write nodes
//    val hash = hashSolution(treeElement.solution)
//    val output = "\"" + hash + "\" [label = \" " + hashProgram(treeElement.solution.expression()).substring(0, 2) + "\"]; \n"
//    pw.write(output)
//
//    if (treeElement.successor.size != 0) {
//      // write successor nodes
//      treeElement.successor.foreach(succ => {
//        writeNodesDot(succ, pw)
//      })
//    }
//  }
//
//  def writeEdgesDot(treeElement: TreeElement[P], pw: PrintWriter): Unit = {
//
//    if (treeElement.successor.size != 0) {
//
//      // write edges to successors
//      treeElement.successor.foreach(succ => {
//
//        val hash = hashSolution(treeElement.solution)
//        val hashSuccessor = hashSolution(succ.solution)
//
//        val output = "\"" + hash + "\" -> \"" + hashSuccessor + "\"  [label = \"" + succ.strategy + "\"]; \n"
//        pw.write(output)
//
//      })
//
//      // write edges of all successors
//      treeElement.successor.foreach(succ => {
//        writeEdgesDot(succ, pw)
//      })
//    }
//  }
//
//  // count
//  def countLeafs(treeElement: TreeElement[P]): Int = {
//    // count this node
//    var counter = 0
//
//    if (treeElement.successor.size != 0) {
//
//      // count successor nodes
//      treeElement.successor.foreach(succ => {
//        counter += countLeafs(succ)
//      })
//      counter
//    } else {
//      counter = 1
//    }
//    counter
//  }
//
//  def countNodes(treeElement: TreeElement[P]): Int = {
//    // count this node
//    var counter = 1
//
//    if (treeElement.successor.size != 0) {
//
//      // count successor nodes
//      treeElement.successor.foreach(succ => {
//        counter += countNodes(succ)
//      })
//      counter
//    }
//    counter
//  }
//
//  // get something
//
//  def leafs(): Seq[TreeElement[P]] = {
//
//    var countedLeaves = new ListBuffer[TreeElement[P]]
//
//    getLeafs(initial)
//
//    def getLeafs(treeElement: TreeElement[P]): Int = {
//      // count this node
//      var counter = 0
//
//      if (treeElement.successor.size != 0) {
//
//        // count successor nodes
//        treeElement.successor.foreach(succ => {
//          counter += getLeafs(succ)
//        })
//        counter
//      } else {
//        // add to elements
//
//        countedLeaves += treeElement
//
//        counter = 1
//      }
//      counter
//    }
//
//    countedLeaves.toSeq
//  }
//
//
//  // write json
//  def toJsonNumbers(filename: String): String = {
//
//    // write string to file
//    val uniqueFilename_full = SearchSpaceHelper.getUniqueFilename(filename, 5)
//    val pw = new PrintWriter(new FileOutputStream(new File(uniqueFilename_full), false))
//
//    val doe = 100
//    val optimizationIterations = 100
//
//    val begin = {
//      s"""{
//      "application_name": "mv_exploration",
//      "optimization_objectives": ["runtime"],
//      "feasible_output" : {
//        "enable_feasible_predictor" : true,
//        "name" : "Valid",
//        "true_value" : "True",
//        "false_value" : "False"
//      },
//      "hypermapper_mode" : {
//        "mode" : "client-server"
//      },
//      "design_of_experiment": {
//        "doe_type": "random sampling",
//        "number_of_samples": ${doe}
//      },
//      "optimization_iterations": ${optimizationIterations},
//      "input_parameters" : {\n"""
//
//    }
//
//
//    var entries = ""
//
//    val json = getConstraints()
//    json.foreach(elem => {
//
//      val dependency: String = elem._1 == 1 match {
//        case true => ""
//        case false => "\"" + s"s${elem._1 - 1}" + "\""
//      }
//
//      entries +=
//        s"""        "s${elem._1}" : {
//           |          "parameter_type" : "ordinal",
//           |          "values" : ${SearchSpaceHelper.strategies.map(x => x._2).toSeq.sorted.mkString("[", ", ", "]")},
//           |          "constraints" : ${elem._2.mkString("[\"", " | ", "\"]")},
//           |          "dependencies" : [${dependency}]
//           |        },
//           |""".stripMargin
//    })
//
//    entries = entries.dropRight(2) + "\n"
//
//    val end =
//      """
//        | }
//        |}
//        |""".stripMargin
//
//
//    pw.write(begin)
//    pw.write(entries)
//    pw.write(end)
//    pw.close()
//
//    uniqueFilename_full
//  }
//
//
//  def getConstraints(): mutable.HashMap[Int, Set[String]] = {
//
//    // generateConstraints
//    val constraints = mutable.HashMap.empty[Int, Set[String]]
//
//    def generateConstraints(treeElement: TreeElement[P]): Unit = {
//
//      val constraint = treeElement.predecessor.strategy match {
//        case null => s"(s${treeElement.layer.toString}" + " == " + SearchSpaceHelper.strategies.apply(treeElement.strategy.toString()) + ")"
//        case _ => s"((s${treeElement.layer.toString}" + " == " + SearchSpaceHelper.strategies.apply(treeElement.strategy.toString()) + ")" + " & " + s"(s${treeElement.predecessor.layer.toString}" + " == " + SearchSpaceHelper.strategies.apply(treeElement.predecessor.strategy.toString()) + "))"
//      }
//
//      val elem: Set[String] = constraints.isDefinedAt(treeElement.layer) match {
//        case true => constraints.apply(treeElement.layer)
//        case false => Set.empty[String]
//      }
//
//      val con: Set[String] = elem ++ (Set(constraint))
//
//      // generate constraint for each elem
//      constraints.addOne(treeElement.layer, con)
//
//      // call this foreach successor
//      treeElement.successor.foreach(succ => {
//        generateConstraints(succ)
//      })
//
//    }
//
//    // generate constraints
//    initial.successor.foreach(succ => {
//      generateConstraints(succ)
//    })
//
//    constraints
//  }
//
//
//  // todo think about this? compare to writeToDisk
//  def writeExpressionsDisk(filename: String) = {
//
//    //    val searchSpace =getSearchSpace()
//    val searchSpace = leafs()
//
//
//    searchSpace.foreach(elem => {
//
//      // hash program
//      val hash = hashProgram(elem.solution.expression())
//
//      // create unique output folder
//      val uniqueFilename = SearchSpaceHelper.getUniqueFilename(filename + "/Expressions/" + hash, 0)
//      // create folder
//      (s"mkdir ${uniqueFilename}" !!)
//
//      // create file for expression
//      val pwProgram = new PrintWriter(new FileOutputStream(new File(uniqueFilename + "/" + hash), false))
//
//      // create file for strategies
//      val pwStrategies = new PrintWriter(new FileOutputStream(new File(uniqueFilename + "/strategies"), false))
//
//      // create file for value todo (add tuning results?)
//      //      val pwValue = new PrintWriter(new FileOutputStream(new File(uniqueFilename + "/value"), false))
//
//      // write expression to file
//      pwProgram.write(elem.solution.expression().toString)
//
//      // strategy list
//      val list = elem.solution.strategies()
//
//      // create strategy string for file
//      var strategyString = ""
//      list.foreach(elem => {
//        elem match {
//          case null => strategyString += "null" + "\n"
//          case _ => strategyString += elem.toString + "\n"
//        }
//      })
//
//      // write strategy string to file
//      pwStrategies.write(strategyString)
//
//      // write value to file
//      //      pwValue.write(elem.value.toString)
//
//      // close files
//      pwProgram.close()
//      pwStrategies.close()
//      //      pwValue.close()
//    })
//  }
//}
