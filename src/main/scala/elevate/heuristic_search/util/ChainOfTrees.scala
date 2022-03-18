//package elevate.heuristic_search.util
//
//import elevate.core.Strategy
//
//import java.io.{File, FileOutputStream, PrintWriter}
//import java.nio.file.{Files, Paths}
//import scala.collection.mutable
//import scala.collection.mutable.ListBuffer
//import scala.language.postfixOps
//import scala.sys.process._
//
//class ChainOfTrees[P] (
//                        var trees: Seq[Tree[P]]
//                      ){
//}
//
//class TreeElement[P] (val solution: Solution[P], // do we need this here?
//                      val strategy: Strategy[P], // node itself
//                      val layer: Int,
//                      val value: Option[Double], // later
//                      val predecessor: TreeElement[P],
//                      var successor: Set[TreeElement[P]], // mutable ? -> tree in scala?
//                     ){
//}
//
//object  cotHelper{
//
//  val strategies: Map[String, Int] = Map(
//    "id" -> 0,
//    "lowerGs0" -> 1,
//    "lowerGs1" -> 2,
//    "lowerWrg0" -> 3,
//    "lowerWrg1" -> 4,
//    "lowerLcl0" -> 5,
//    "lowerLcl1" -> 6,
//    "lowerGsGs" -> 7,
//    "lowerWrgLcl" -> 8,
//    "lowerWrgWrgLclLcl" -> 9,
//    // split join
//    "allSplitJoin" -> 10,
//    "oneSplitJoin" -> 11,
//    "someSplitJoin" -> 12,
//    "oneUsingStateSplitJoin" -> 13,
//    "topDownSplitJoin" -> 14,
//    "allTopdownSplitJoin" -> 15,
//    "bottomUpSplitJoin" -> 16
//  )
//
//
//  def getStrategies(numbers: Seq[Int]): Seq[String] = {
//    val strategyBuffer = new ListBuffer[String]
//    numbers.foreach(number => {
//      strategyBuffer += cotHelper.strategies.map(_.swap).apply(number)
//    })
//
//    strategyBuffer.toSeq
//  }
//
//}
//
//class Tree[P] (
//                val initial: TreeElement[P], // successors on top level
//
//) {
//
//
//
//  def getUniqueFilename(filename:String, offset: Int):String= {
//  var uniqueFilename_full = filename
//
//    // check if file or folder already exists
//    if(Files.exists(Paths.get(uniqueFilename_full))){
//      //val warningString = "Warning! Clash at " + uniqueFilename_full + ".\n"
//      //println(warningString + "adding System.currentTimeMillis().")
//
//      // wait for it
//      Thread.sleep(1)
//
//      // append timestamp
//      val end = uniqueFilename_full.substring(uniqueFilename_full.length-offset, uniqueFilename_full.length)
//      uniqueFilename_full = uniqueFilename_full.substring(0, uniqueFilename_full.length-offset)+ "_" + System.currentTimeMillis() + end
//    }
//
//    uniqueFilename_full
//  }
//  def printTree(): Unit = {
//
////    printNode(initial)
//
//  }
//
//  def toJsonNumbers(filename: String): String = {
//
//    // write string to file
//    val uniqueFilename_full = getUniqueFilename(filename, 5)
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
//        case false => "\"" + s"s${elem._1-1}" + "\""
//      }
//
//      entries +=
//        s"""        "s${elem._1}" : {
//           |          "parameter_type" : "ordinal",
//           |          "values" : ${cotHelper.strategies.map(x => x._2).toSeq.sorted.mkString("[", ", ", "]")},
//           |          "constraints" : ${elem._2.mkString("[\"", " | ", "\"]")},
//           |          "dependencies" : [${dependency}]
//           |        },
//           |""".stripMargin
//  })
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
//    println(begin + entries + end)
//    pw.write(begin)
//    pw.write(entries)
//    pw.write(end)
//    pw.close()
//
//    uniqueFilename_full
//  }
//
//  def getConstraints(): mutable.HashMap[Int, Set[String]] = {
//
//    // generateConstraints
//    val constraints = mutable.HashMap.empty[Int, Set[String]]
//    def generateConstraints(treeElement: TreeElement[P]): Unit = {
//
//      val constraint = treeElement.predecessor.strategy match {
//        case null => s"(s${treeElement.layer.toString}" + " == " + cotHelper.strategies.apply(treeElement.strategy.toString()) + ")"
//        case _ => s"((s${treeElement.layer.toString}" + " == " + cotHelper.strategies.apply(treeElement.strategy.toString()) + ")" + " & " + s"(s${treeElement.predecessor.layer.toString}" + " == " + cotHelper.strategies.apply(treeElement.predecessor.strategy.toString()) + "))"
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
//  def toJsonStrings(filename: String): String= {
//
//    // write string to file
//    val uniqueFilename_full = getUniqueFilename(filename, 5)
//    val pw = new PrintWriter(new FileOutputStream(new File(uniqueFilename_full), false))
//
//    val doe = 10
//    val optimizationIterations = 10
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
//    val strategies = Seq(
//      "lowerGs0",
//      "lowerGs1",
//      "lowerWrg0",
//      "lowerWrg1",
//      "lowerLcl0",
//      "lowerLcl1",
//      "lowerGs0",
//      "lowerGsGs",
//      "lowerWrgLcl",
//      "lowerWrgWrgLclLcl",
//      // split join
//      "allSplitJoin",
//      "oneSplitJoin",
//      "someSplitJoin",
//      "oneUsingStateSplitJoin",
//      "topDownSplitJoin",
//      "allTopdownSplitJoin",
//      "bottomUpSplitJoin"
//    )
//
//    var entries = ""
//
//    val json = getConstraintsString()
//    json.foreach(elem => {
//
//      val dependency: String = elem._1 == 1 match {
//        case true => ""
//        case false => "\"" + s"s${elem._1-1}" + "\""
//      }
//
//      entries +=
//        s"""        "s${elem._1}" : {
//           |          "parameter_type" : "categorical",
//           |          "values" : ${strategies.mkString("[\"", "\", \"", "\"]")},
//           |          "constraints" : ${elem._2.mkString("[\"", "\", \"", "\"]")},
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
//    println(begin + entries + end)
//    pw.write(begin)
//    pw.write(entries)
//    pw.write(end)
//    pw.close()
//
//
//    uniqueFilename_full
//  }
//
//  def getConstraintsString(): mutable.HashMap[Int, Set[String]] = {
//
//    // generateConstraints
//    val constraints = mutable.HashMap.empty[Int, Set[String]]
//    def generateConstraints(treeElement: TreeElement[P]): Unit = {
//
//      val constraint = treeElement.predecessor.strategy match {
//        case null => s"s${treeElement.layer.toString}" + " == \'" + treeElement.strategy + "\'"
//        case _ => s"s${treeElement.layer.toString}" + " == \'" + treeElement.strategy + "\' & " + s"s${treeElement.predecessor.layer.toString}" + " == \'" + treeElement.predecessor.strategy + "\'"
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
//  def treeSize(): Int = {
//    countNodes(initial)
//  }
//
//  def leafSize(): Int = {
//    countLeafs(initial)
//  }
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
//      if(treeElement.successor.size != 0){
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
//  def countLeafs(treeElement: TreeElement[P]): Int = {
//    // count this node
//    var counter = 0
//
//    if(treeElement.successor.size != 0){
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
//    if(treeElement.successor.size != 0){
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
//
//
//  def writeNodesDot(treeElement: TreeElement[P], pw: PrintWriter): Unit = {
//
//
//      // write nodes
//      val hash = hashSolution(treeElement.solution)
//      val output = "\"" + hash + "\" [label = \" " + hashProgram(treeElement.solution.expression).substring(0, 2) + "\"]; \n"
//      pw.write(output)
//
//    if(treeElement.successor.size != 0) {
//      // write successor nodes
//      treeElement.successor.foreach(succ => {
//          writeNodesDot(succ, pw)
//      })
//    }
//  }
//
//  def writeEdgesDot(treeElement: TreeElement[P], pw: PrintWriter): Unit = {
//
//    if(treeElement.successor.size != 0) {
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
//          writeEdgesDot(succ, pw)
//      })
//    }
//  }
//
//  def writeTreeDot(filename: String): Unit = {
//    // write string to file
//    val uniqueFilename_full = getUniqueFilename(filename, 4)
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
//  }
//}
//
