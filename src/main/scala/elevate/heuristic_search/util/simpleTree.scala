package elevate.heuristic_search.util
import elevate.core.Strategy

import java.io.{File, FileOutputStream, PrintWriter}
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

import scala.collection.parallel.CollectionConverters._
import scala.collection.parallel.ParSet

import scala.language.postfixOps
import scala.sys.process._

// try to rewrite this
class SimpleTreeElement[P] (
                             val solutionHash: String, // maybe remove this
                             val rewrite: Int, // 0 - 16
                             val layer: Int, // Do we need this?
                             val predecessor: SimpleTreeElement[P],
                             var successor: Set[SimpleTreeElement[P]]
                           ) {

  // get rewrites from strucute

}

class SimpleTree[P] (val initial: SimpleTreeElement[P],
                     val initialSolution: Solution[P]) extends SearchSpace[P]{

  // we do not need add here
  override def add(program: P, strategy: Strategy[P], value: Option[Double]): Unit = ???

  def getRewriteNumbers(simpleTreeElement: SimpleTreeElement[P]): Seq[Int] = {

    // get rewrites by backtracking of tree elements
    var tmp = simpleTreeElement
    val rewrites = new ListBuffer[Int]

    // we can drop rewrite of root
    while(tmp.predecessor != null){

      // add rewrites
      rewrites += tmp.rewrite

      // next element
      tmp = tmp.predecessor
    }

    rewrites.toSeq.reverse
  }


  override def printConsole(): Unit = {
    printNode(initial)
  }

  override def getSizeTotal(): Int = {
    countNodes(initial)
  }

  override def getSize(): Int = {
    val countStart = System.currentTimeMillis()
    val result = countLeafs(initial)
    val duration = System.currentTimeMillis() - countStart
    print("getSize: " + duration.toDouble/1000 + "s - ")

    result
  }

  // todo fix this
  override def getSearchSpace(): Seq[Solution[P]] = {
    Seq.empty[Solution[P]]
  }
  override def getElement(i: Int): SearchSpaceElement[P] = ???

//  override def getSearchSpace(): Seq[Solution[P]] = {
//    leafs().map(x => x.solution)
//  }

  // warning! can be slow
//  override def getElement(i: Int): SearchSpaceElement[P] = {
//    leafs().apply(i)
//  }

  override def writeToDot(filename: String): String = {
    // write string to file
    val uniqueFilename_full = SearchSpaceHelper.getUniqueFilename(filename, 4)
    val pw = new PrintWriter(new FileOutputStream(new File(uniqueFilename_full), false))

    val begin = "strict digraph path {\n"
    pw.write(begin)
    println("writen nodes")
    writeNodesDot(initial, pw)
    println("write edges")
    writeEdgesDot(initial, pw)
    val end = "}"
    pw.write(end)
    pw.close()

    // visualize dot graph
          (s"dot -Tpng -O " + uniqueFilename_full !!)
          (s"dot -Tpdf -O " + uniqueFilename_full !!)

    ""
  }

  override def writeToDisk(filename: String): Unit = {

    // save expressions
    // todo write expression to disk
    writeExpressionsDisk(filename)

    // save json for export
    //    toJsonNumbers(filename)
    // todo make this more generic, currently json is written and copied elsewhere

  }

  // helper

  // export
  def printNode(simpleTreeElement: SimpleTreeElement[P]): Unit = {

    println("treeElement: " + simpleTreeElement.rewrite)
    println("layer: " + simpleTreeElement.layer)
    if(simpleTreeElement.predecessor == null){
      println("predecessor: " + "null")
    } else {
      println("predecessor: " + simpleTreeElement.predecessor.rewrite)
    }
    println("successor: " + simpleTreeElement.successor.toSeq.map(elem => elem.rewrite).mkString("[", ", ", "]"))
    println("\n")

    simpleTreeElement.successor.foreach(succ => {
      printNode(succ)
    })
  }

  def writeNodesDot(simpleTreeElement: SimpleTreeElement[P], pw: PrintWriter): Unit = {

    // create hash
    val numbers = getRewriteNumbers(simpleTreeElement)
    val hash = hashAndNumbers(simpleTreeElement.solutionHash, numbers)

    // write nodes
    val output = "\"" + hash + "\" [label = \" " + simpleTreeElement.solutionHash.substring(0, 2) + "\"]; \n"
    pw.write(output)

    if(simpleTreeElement.successor.size != 0) {
      // write successor nodes
      simpleTreeElement.successor.foreach(succ => {
        writeNodesDot(succ, pw)
      })
    }
  }

  def writeEdgesDot(simpleTreeElement: SimpleTreeElement[P], pw: PrintWriter): Unit = {

    if(simpleTreeElement.successor.size != 0) {

      // write edges to successors
      simpleTreeElement.successor.foreach(succ => {

        val hash = hashAndNumbers(simpleTreeElement.solutionHash, getRewriteNumbers(simpleTreeElement))
        val hashSuccessor = hashAndNumbers(succ.solutionHash, getRewriteNumbers(succ))

        val output = "\"" + hash + "\" -> \"" + hashSuccessor + "\"  [label = \"" + SearchSpaceHelper.strategies.map(_.swap).apply(succ.rewrite) + "\"]; \n"
        pw.write(output)

      })

      // write edges of all successors
      simpleTreeElement.successor.foreach(succ => {
        writeEdgesDot(succ, pw)
      })
    }
  }

  // count
  def countLeafs(simpleTreeElement: SimpleTreeElement[P]): Int = {
    // count this node
    var counter = 0

    if(simpleTreeElement.successor.size != 0){

      // count successor nodes
      simpleTreeElement.successor.foreach(succ => {
        counter += countLeafs(succ)
      })
      counter
    } else {
      counter = 1
    }
    counter
  }

  def countNodes(simpleTreeElement: SimpleTreeElement[P]): Int = {
    // count this node
    var counter = 1

    if(simpleTreeElement.successor.size != 0){

      // count successor nodes
      simpleTreeElement.successor.foreach(succ => {
        counter += countNodes(succ)
      })
      counter
    }
    counter
  }

  // get something

  def leafs(): Seq[SimpleTreeElement[P]] = {

    var countedLeaves = new ListBuffer[SimpleTreeElement[P]]

    getLeafs(initial)

    def getLeafs(simpleTreeElement: SimpleTreeElement[P]): Int = {
      // count this node
      var counter = 0

      if(simpleTreeElement.successor.size != 0){

        // count successor nodes
        simpleTreeElement.successor.foreach(succ => {
          counter += getLeafs(succ)
        })
        counter
      } else {
        // add to elements

        countedLeaves += simpleTreeElement

        counter = 1
      }
      counter
    }

    countedLeaves.toSeq
  }


  // write json
  def toJsonNumbers(filename: String): String = {

    // write string to file
    val uniqueFilename_full = SearchSpaceHelper.getUniqueFilename(filename, 5)
    val pw = new PrintWriter(new FileOutputStream(new File(uniqueFilename_full), false))

    val doe = 5
    val optimizationIterations = 5


    val begin = {
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
      "optimization_method": "exhaustive",
      "input_parameters" : {\n"""
    }
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
//      "design_of_experiment" : {
//        "doe_type" : "random sampling",
//        "number_of_samples" : ${doe}
//      },
//      "scalarization_method": "linear",
//      "models": {
//        "model": "gaussian_process"
//       },
//      "optimization_method": "bayesian_optimization",
//      "optimization_iterations" : ${optimizationIterations},
//      "input_parameters" : {\n"""
//    }
//

    var entries = ""

    val json = getConstraints()
    json.foreach(elem => {

      val dependencies = new ListBuffer[String]

      var layer = elem._1
      while(layer > 1) {
        dependencies += ("\"" + s"s${layer-1}" + "\"")
        layer -= 1
      }

//      |          "values" : ${SearchSpaceHelper.strategies.map(x => x._2).toSeq.sorted.mkString("[", ", ", "]")},

      entries +=
        s"""        "s${elem._1}" : {
           |          "parameter_type" : "ordinal",
           |          "values" : ${Seq(0, 1, 2, 3).sorted.mkString("[", ", ", "]")},
           |          "constraints" : ${elem._2.mkString("[\"", " | ", "\"]")},
           |          "dependencies" : ${dependencies.toSeq.sorted.mkString("[", ", ", "]")}
           |        },
           |""".stripMargin
    })

    entries = entries.dropRight(2) + "\n"

    val end =
      """
        | }
        |}
        |""".stripMargin


    pw.write(begin)
    pw.write(entries)
    pw.write(end)
    pw.close()

    uniqueFilename_full
  }




  // WARNING: blows config file, because we store each complete edge as constraint
  // todo filter out duplicates
  def getConstraints(): mutable.HashMap[Int, Set[String]] = {

    // generateConstraints
    val constraints = mutable.HashMap.empty[Int, Set[String]]
    def generateConstraints(simpleTreeElement: SimpleTreeElement[P]): Unit = {

      var iteratorNode = simpleTreeElement
      val collectedConstraints = new ListBuffer[String]
      while(iteratorNode.rewrite >= 0){
        // process
        val constraint = iteratorNode.predecessor.rewrite match {
          case -1 => s"(s${iteratorNode.layer.toString}" + " == " + iteratorNode.rewrite + ")"
          case _ => s"((s${iteratorNode.layer.toString}" + " == " + iteratorNode.rewrite + ")" + " & " + s"(s${iteratorNode.predecessor.layer.toString}" + " == " + iteratorNode.predecessor.rewrite + "))"
        }

        collectedConstraints += constraint

        iteratorNode = iteratorNode.predecessor
      }

      //      val constraintsString = collectedConstraints.toSet
      //      val constraintsString2 = collectedConstraints.toSet.mkString(" & ")
      //      println("constraintsString: " + constraintsString)
      //      println("constraintsString2: " + constraintsString2)


      val elem: Set[String] = constraints.isDefinedAt(simpleTreeElement.layer) match {
        case true => constraints.apply(simpleTreeElement.layer)
        case false => Set.empty[String]
      }

      val con: Set[String] = elem ++ Set(collectedConstraints.toSet.mkString(" & "))

      // generate constraint for each elem
      constraints.addOne(simpleTreeElement.layer, con)

      // call this foreach successor
      simpleTreeElement.successor.foreach(succ => {
        generateConstraints(succ)
      })

    }

    // generate constraints
    initial.successor.foreach(succ => {
      generateConstraints(succ)
    })

    constraints
  }


  def toJsonNumbers2(filename: String): String = {


    // write string to file
    val uniqueFilename_full = SearchSpaceHelper.getUniqueFilename(filename, 5)
    val pw = new PrintWriter(new FileOutputStream(new File(uniqueFilename_full), false))

    val doe = 5
    val optimizationIterations = 5

    val begin = {
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
      "design_of_experiment" : {
        "doe_type" : "random sampling",
        "number_of_samples" : ${doe}
      },
      "scalarization_method": "linear",
      "models": {
        "model": "gaussian_process"
       },
      "optimization_method": "bayesian_optimization",
      "optimization_iterations" : ${optimizationIterations},
      "input_parameters" : {\n"""
    }


    var entries = ""


    // todo make this bottom up
    val json = getConstraints2()

//    println("constraints: " + json.)

    val jsonSize = json.toSeq.map(elem => elem._2.size).reduceLeft((a,b) => a+b)
    println("constraints: " + jsonSize)

//
//    entries +=
//      s"""        "s${0}" : {
//         |          "parameter_type" : "ordinal",
//         |          "values" : ${SearchSpaceHelper.strategies.map(x => x._2).toSeq.sorted.mkString("[", ", ", "]")},
//         |          "constraints" : ${elem._2.mkString("[\"", " | ", "\"]")},
//         |          "dependencies" : ${dependencies.toSeq.sorted.mkString("[", ", ", "]")}
//         |        },
//         |""".stripMargin


    // todo check if this works for bottom up order
    json.foreach(elem => {

      val dependencies = new ListBuffer[String]

      val max = json.map(elem => elem._1).max

      var layer = elem._1
      while(layer < max) {
        dependencies += ("\"" + s"s${layer+1}" + "\"")
        layer += 1
      }

      entries +=
        s"""        "s${elem._1}" : {
           |          "parameter_type" : "ordinal",
           |          "values" : ${SearchSpaceHelper.strategies.map(x => x._2).toSeq.sorted.mkString("[", ", ", "]")},
           |          "constraints" : ${elem._2.mkString("[\"", " | ", "\"]")},
           |          "dependencies" : ${dependencies.toSeq.sorted.mkString("[", ", ", "]")}
           |        },
           |""".stripMargin
    })

    entries = entries.dropRight(2) + "\n"

    val end =
      """
        | }
        |}
        |""".stripMargin


    pw.write(begin)
    pw.write(entries)
    pw.write(end)
    pw.close()

    uniqueFilename_full
  }

  def toJsonNumbers3(filename: String): String = {


    // write string to file
    val uniqueFilename_full = SearchSpaceHelper.getUniqueFilename(filename, 5)
    val pw = new PrintWriter(new FileOutputStream(new File(uniqueFilename_full), false))

    val doe = 5
    val optimizationIterations = 5

    val begin = {
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
      "optimization_method": "exhaustive",
      "input_parameters" : {\n"""
    }


    var entries = ""


    // todo make this bottom up
    val json = getConstraintsInvert()

    //    println("constraints: " + json.)

    val jsonSize = json.toSeq.map(elem => elem._2.size).reduceLeft((a,b) => a+b)
    println("constraints: " + jsonSize)

    //
    //    entries +=
    //      s"""        "s${0}" : {
    //         |          "parameter_type" : "ordinal",
    //         |          "values" : ${SearchSpaceHelper.strategies.map(x => x._2).toSeq.sorted.mkString("[", ", ", "]")},
    //         |          "constraints" : ${elem._2.mkString("[\"", " | ", "\"]")},
    //         |          "dependencies" : ${dependencies.toSeq.sorted.mkString("[", ", ", "]")}
    //         |        },
    //         |""".stripMargin


    // todo check if this works for bottom up order
    json.foreach(elem => {

//      val dependencies = new ListBuffer[String]

//      val max = json.map(elem => elem._1).max
//
//
//      var layer = elem._1
//      while(layer < max) {
//        dependencies += ("\"" + s"s${layer+1}" + "\"")
//        layer += 1
//      }


      val dependencies = new ListBuffer[String]

      var layer = elem._1
      while(layer > 1) {
        dependencies += ("\"" + s"s${layer-1}" + "\"")
        layer -= 1
      }

//      |           |          "values" : ${SearchSpaceHelper.strategies.map(x => x._2).toSeq.sorted.mkString("[", ", ", "]")},

      entries +=
        s"""        "s${elem._1}" : {
           |          "parameter_type" : "ordinal",
           |          "values" : ${Seq(0, 1, 2, 3).sorted.mkString("[", ", ", "]")},
           |          "constraints" : ${elem._2.toSeq.sorted.mkString("[\"", "\", \"", "\"]")},
           |          "dependencies" : ${dependencies.toSeq.sorted.mkString("[", ", ", "]")}
           |        },
           |""".stripMargin

    })

    entries = entries.dropRight(2) + "\n"

    val end =
      """
        | }
        |}
        |""".stripMargin


    pw.write(begin)
    pw.write(entries)
    pw.write(end)
    pw.close()

    uniqueFilename_full
  }

  def getConstraintsInvert(): mutable.HashMap[Int, Set[String]] = {
    var constraints = mutable.HashMap.empty[Int, Set[String]]


    val layerLimit = 5
    val strategies = 4
//    val strategies = 4


    def all(numbers: Seq[Int]): Seq[Seq[Int]] = {
//      println("call all: " + numbers.mkString("[", ",", "]"))
//      println("progress: " + numbers.size + "/" + layerLimit)

      var output = scala.collection.mutable.Seq.empty[Seq[Int]]

      for (j <- Range(0, strategies)) {
        val numbersCombined = numbers :+ j
        output = output :+ numbersCombined
      }

      if(numbers.size+1 < layerLimit) {
        var output2 = scala.collection.mutable.Seq.empty[Seq[Int]]
        output.toSeq.foreach(elem => {
          output2 = output2 ++ all(elem)
        })
        output2.toSeq
      }else{
        output.toSeq
      }
    }
    // todo collect all constraints/inverted
    // collect all invalid paths

    // extensible -> flip around
    // s1*1 + s2*100 + s3*1000 != 001404 -> rewrite [4, 14, 0] (invalid rewrite)
    // s1*1 + s2*100 + s3*1000 + s4 * 100000 != 03001404 -> rewrite [4, 14, 0, 03] (invalid rewrite)

    // top down?
    // for all invalid children -> get invalid ones?


    // get all valid numbers => then create all invalid numbers from that

    // get rewrite sequence for each leaf

//     doesn't get the leafs here !

    println("leafs: " + leafs().size)

    val numbers = leafs().map(elem => {
      var tmp = elem
      val numbers = new ListBuffer[Int]
      while(tmp.rewrite >= 0){
        numbers += tmp.rewrite
        tmp = tmp.predecessor
      }
      numbers.toSeq
    })

//    numbers.foreach(number => {
//      println(number.mkString("[", "," , "]"))
//    })

    // invert

    println("create all numbers")
//    val allNumbers2 = SearchSpaceHelper.strategies.map(elem => elem._2).toSeq.par.map(elem => all(Seq(elem))).seq
    val allNumbers2 = Seq(0, 1, 2, 3).par.map(elem => all(Seq(elem))).seq
    println("finished all numbers")


    println("now flatten")
    val allNumbers = allNumbers2.flatten
//    val allNumbers = allNumbers2.reduce((A,B) => A ++ B)
    println("finished flatten ")


    println("\n")

//    numbers.foreach(elem => println(elem.size))

    println("now filter")
//    val filtered_tmp = allNumbers.par.filter(elem => !numbers.contains(elem)).seq
    val filtered_tmp = allNumbers.par.filter(elem => !elem.forall(a => a % 2 == 0)).seq
//    val filtered_tmp = allNumbers
    println("finished filtering")



    def fullChildren(node: Seq[Int]): Set[Seq[Int]] = {

      val children = Range(0, strategies).map(child => node.appended(child)).toSet

      if(node.size < layerLimit - 1){
        children.par.map(child => fullChildren(child)).flatten.seq
      } else {
        children
      }
    }

    def getChildren(numbers: Seq[Seq[Int]], node: Seq[Int]): Seq[Seq[Int]] = {

      if (node.size < layerLimit) {

        // get full children
        val children2 = fullChildren(node)

        // filter out existing children
        numbers.par.intersect(children2.toSeq).seq
      }else{
        Seq.empty[Seq[Int]]
      }
    }

    def mergeConstraints(numbers3: Set[Seq[Int]], numbers: Seq[Seq[Int]], node: Seq[Int]): Seq[Seq[Int]] = {

      if(node.size < layerLimit){

      val children = getChildren(numbers, node)
      val remaining = layerLimit - node.size
      val full = math.pow(strategies, remaining).toInt

      var numbers2 = scala.collection.mutable.Set.empty[Seq[Int]]
      numbers2 = numbers2 ++ numbers3.toSet

      if(children.size == full){
        // remove children from numbers
        children.foreach(child => {

          // remove child from numbers2
          numbers2.remove(child)
        })

        // add merged constraint to numbers2
        numbers2 += (node.appended(-1))

      } else {

        // add children as constraint
        if(node.size < layerLimit) {
          children.foreach(child => {
            numbers2 += child
          })
        }
        if(node.size + 1 < layerLimit) {
        children.toSet.map((child2: Seq[Int]) => child2.take(node.size + 1)).foreach(child => {
          numbers2 = mergeConstraints(numbers2.toSet, numbers, child).to(collection.mutable.Set)
        })
        }
      }
        numbers2.toSeq
      }else{
        Seq.empty[Seq[Int]]
      }
    }

    // merge constraints
    // call  this her e
    println("now merge constraints for: " + filtered_tmp.size)
    val mc = mergeConstraints(Set.empty[Seq[Int]], filtered_tmp, Seq())

//    val diff = allNumbers.diff(filtered_tmp) ??

    println()

    println("allNumbers: " + allNumbers.size)
//    println("numbers: " + numbers.size)
    println("filtered_tmp: " + filtered_tmp.size)

//    println("numbers: " + numbers.mkString(","))
//    println("allNumbers:   " + allNumbers.mkString(","))
//    println("filtered_tmp: " + filtered_tmp.mkString(","))
//    println("diff:         " + diff.mkString(","))
    println("mc:           " + mc.mkString(","))
//    println("mc2: " + mc2.mkString(","))

    //    println(allNumbers.mkString("[", ", ", "]"))
    println("mergedConstraints: " + mc.size)
//    println("mergedConstraints2 ( should be 0): " + mc2.size)

    println("\n")


    // filter all uneven values out
    println("now filter2")
//    val filtered = filtered_tmp.par.filter(elem => elem.forall(value => value % 2 == 0)).seq
    println("finished filter2")

    println("\n")

    println("allNumbers.size: " + allNumbers.size)
//    println("numbers: " + numbers.size)
    println("filtered_tmp: " + filtered_tmp.size)
//    println("filtered: " + filtered.size)
//    println("difference: " + (allNumbers.size - numbers.size).toString)

    // write constraints for filtered individually
    // warning! huge amount 83 thousand!

    def writeConstraints(constraints: Seq[Seq[Int]], index: Int): mutable.HashMap[Int, Set[String]] = {

      val filtered = constraints.map(elem => elem.take(index)).toSet

      // todo think about that
      // add only if next value is fully covered?
      // single constraint? if now  rewrite at all on this layer -> constraint
      // handle constraints on layer individually?


      println("filtered.size: " + filtered.size)
//      filtered.size

      // [0, 1, 2, 3, 4] -> constraint
      // todo fix that
      var result = mutable.HashMap.empty[Int, Set[String]]
//      val result: mutable.HashMap[Int, Set[String]] = filtered.map(elem => {
        filtered.foreach(elem => {
          println("elem: " + elem.mkString(","))

        // make constraint from element

        // s1*1 + s2*100 + s3*1000 != 001404 -> rewrite [4, 14, 0] (invalid rewrite)
        //
        var counter = -1
        var counter2 = -2
//        var constraintString = scala.collection.mutable.Seq.empty[String]

        val elem2 = elem.last match {
          case -1 => elem.dropRight(1)
          case _ => elem//
        }

//        val elem2 = elem.dropRight(1)

        val constraintString = elem2.map(value => {
          counter += 1
          counter2 += 2
          (s"s${counter+1}*${math.pow(10, counter2)}",
            math.pow(10, counter2)*value)
        })


          println("constraintString: " + constraintString.mkString(","))

////
//          constraintString.foreach(constraintsStringElement => {
//
//            // layer
//            val index2 = constraintsStringElement._3
//
//            val resultString2: Set[String] = result.isDefinedAt(index2) match {
//              case true =>
//                val test = result.apply(index2)
//                val result2 = test ++ Set(constraintString.map(x => x._1).mkString("+") + "!=" + constraintString.map(x => x._2).sum.toString)
//
//                result2
//              case false =>
//                Set(constraintString.map(x => x._1).mkString("+") + "!=" + constraintString.map(x => x._2).sum.toString)
//            }
//
//            result.addOne(index, resultString2)
//          })

          val index2 = constraintString.size

          val resultString: Set[String] = result.isDefinedAt(index2) match {
            case true => {
              val test = result.apply(index2)
//              println("test: " + test.mkString(","))
//              val string = constraintString.map(x => x._1).mkString("+") + "!=" + constraintString.map(x => x._2).sum.toString
//              println("stirng: " + string)
              val result2 = test ++ Set(constraintString.map(x => x._1).mkString("+") + "!=" + constraintString.map(x => x._2).sum.toString)

//              println("result2: " + result2.mkString(","))

              result2
            }
            case false => {
              Set(constraintString.map(x => x._1).mkString("+") + "!=" + constraintString.map(x => x._2).sum.toString)
            }
          }
          result.addOne(index2, resultString)
        })

      println("result.size: " + result.size)

      result
    }


//    val constraints0 = writeConstraints(filtered, 0)
//    println("constraints0: " + constraints0.size)
//    val constraints1 = writeConstraints(filtered, 1)
//    println("constraints1: " + constraints1.size)
//    val constraints2 = writeConstraints(filtered, 2)
//    println("constraints2: " + constraints2.size)
//    val constraints3 = writeConstraints(filtered, 3)
//    println("constraints3: " + constraints3.size)
//    val constraints4 = writeConstraints(filtered, 4)
//    println("constraints4: " + constraints4.size)


    println("create constraints")
//    val filtered = filtered_tmp
    val filtered = mc
    val maxlayer = filtered.maxBy(a => a.size).size
    println("maxLayer: " + maxlayer)

//    filtered.((a,b) => math.max(a.size, b.size))

    // writeConstraints() for each layer in hashmap

    var layer: Int = 1
//    while(layer <= maxlayer){
      val output = writeConstraints(filtered, maxlayer)
      println("output: " + output.size)
      println("output: " + output.mkString(","))
//      constraints.addOne(layer, writeConstraints(filtered, layer))

      println("constraints: " + constraints.mkString(","))

      constraints = output.map(elem => {
        println("elem: " + elem._1)
        val result = constraints.isDefinedAt(elem._1) match {
          case true => (elem._1, constraints.apply(elem._1) ++ elem._2)
          case false =>  (elem._1, elem._2)
        }
        result
      })
      println("constraints: " + constraints.mkString(","))
      println("")

//      constraints.addOne(layer, output)
//      constraints.addOne(layer, writeConstraints(allNumbers, layer))
//      layer += 1
//    }

//    constraints.apply(1).size

//    val max2 = constraints.size
//    var counter2 = 1
//    var sum = 0
//    while(counter2 <= max2){
//      val value = constraints.apply(counter2).size
//      println("layer: " + counter2 + " - " + value)
//      sum += value
//      counter2 += 1
//    }
//
//    println("total: " + sum )

    println("constraints finished ")

//    val sizeTotal = constraints.map(elem => elem._2).reduceLeft((a,b) => a.size + b.size)(0)


    // write constraints to json

//    println(constraints0.mkString("[", ", ", "]"))
//    println(constraints1.mkString("[", ", ", "]"))
//    println(constraints2.mkString("[", ", ", "]"))
//    println(constraints3.mkString("[", ", ", "]"))


    // filter stuff out? -> generalise constraints?


//    val allNumbers2 = allNumbers.filter(elem => elem.size == 3)
//
//    println("allNumbers2.size: " + allNumbers2.toSet.size)
//    allNumbers.foreach(elem =>{
//      println("elem: " + elem)
//    })

    // generate all
//    val layerLimit = 3
//    val strategies = SearchSpaceHelper.strategies.size
//    val localNumbers = SearchSpaceHelper.strategies.map(x => x._2).toSeq



    // filter out valid ones


//    val layer = 1
//
//    val limit = 2

//    val numbers = SearchSpaceHelper.strategies.map(x => x._2).toSeq
//    val rewrites = initial.successor.map(elem => elem.rewrite).toSeq
//    val invalidNumbers = numbers.filter(elem => !rewrites.contains(elem))
//
//    invalidNumbers.foreach(invalidNumber => {

      // invalidNumber -> get all successors?



//
//    var counter = 0
//    while(counter < limit){
//
//      // get rewrites
//
//      // walk down until limit
//
//
//     counter += 1
//    }
//
//    // write constraints
//    val constraintsLayer = invalidNumbers.map(number => {
//      s"s${layer}*1" + "!=" + number
//    })
//
//    println("contraints on layer: " + layer )
//    constraintsLayer.foreach(println)
//
//    constraints
    constraints
  }


  def getConstraints2(): mutable.HashMap[Int, Set[String]] = {

    // generateConstraints
    val constraints = mutable.HashMap.empty[Int, Set[String]]
    def generateConstraints2(simpleTreeElement: SimpleTreeElement[P]): Unit = {

      var iteratorNode = simpleTreeElement
      val collectedConstraints = new ListBuffer[String]
//      while(iteratorNode.rewrite >= 0){

      val constraint = iteratorNode.successor.size match {
        case 0 => s"(s${iteratorNode.layer.toString}" + " == " + iteratorNode.rewrite + ")"
        case _ => {
          iteratorNode.successor.map(succ => {
            s"((s${iteratorNode.layer.toString}" + " == " + iteratorNode.rewrite + ")" + " & " + s"(s${succ.layer.toString}" + " == " + succ.rewrite + "))"
          }).mkString(" | ")
        }
      }

      // flip constraints?



        // process
//        val constraint = iteratorNode.predecessor.rewrite match {
//          case -1 => s"(s${iteratorNode.layer.toString}" + " == " + iteratorNode.rewrite + ")"
//          case _ => s"((s${iteratorNode.layer.toString}" + " == " + iteratorNode.rewrite + ")" + " & " + s"(s${iteratorNode.predecessor.layer.toString}" + " == " + iteratorNode.predecessor.rewrite + "))"
//        }

        collectedConstraints += constraint

//        iteratorNode = iteratorNode.predecessor
//      }

      //      val constraintsString = collectedConstraints.toSet
      //      val constraintsString2 = collectedConstraints.toSet.mkString(" & ")
      //      println("constraintsString: " + constraintsString)
      //      println("constraintsString2: " + constraintsString2)


      // todo check duplicates here!

      val elem: Set[String] = constraints.isDefinedAt(simpleTreeElement.layer) match {
        case true => constraints.apply(simpleTreeElement.layer)
        case false => Set.empty[String]
      }

      val con: Set[String] = elem ++ Set(collectedConstraints.toSet.mkString(" | "))

      // generate constraint for each elem
      constraints.addOne(simpleTreeElement.layer, con)

      if(simpleTreeElement.layer > 1){
        // call this foreach successor
        generateConstraints2(simpleTreeElement.predecessor)
      }
    }

    // constraint for leaf

    // constraints for element and children?

    val treeLeafs = leafs()

    // start from each leaf and write constraints

    // call foreach parent function

    // get layer function? -> generate constraints foreach layer

    // generate constraints

    treeLeafs.foreach(elem => generateConstraints2(elem))

    constraints
  }


  // todo think about this? compare to writeToDisk
  def writeExpressionsDisk(filename: String) = {

    //    val searchSpace =getSearchSpace()
    val searchSpace = leafs()



    searchSpace.foreach(elem => {

      // hash program
//      val hash = hashProgram(elem.solution.expression)
      val hash = elem.solutionHash

      // create unique output folder
      val uniqueFilename = SearchSpaceHelper.getUniqueFilename(filename + "/Expressions/" + hash , 0)
      // create folder
      (s"mkdir ${uniqueFilename}" !!)

      // create file for expression
      val pwProgram = new PrintWriter(new FileOutputStream(new File(uniqueFilename + "/" + hash), false))

      // create file for strategies
      val pwStrategies = new PrintWriter(new FileOutputStream(new File(uniqueFilename + "/strategies"), false))

      // create file for value todo (add tuning results?)
      //      val pwValue = new PrintWriter(new FileOutputStream(new File(uniqueFilename + "/value"), false))

      // write expression to file
      // reproduce elem here
      pwProgram.write(elem.solutionHash)

      // strategy list
      val list = getRewriteNumbers(elem)

      // create strategy string for file
      val strategyString = list.mkString("\n")

      // write strategy string to file
      pwStrategies.write(strategyString)

      // write value to file
      //      pwValue.write(elem.value.toString)

      // close files
      pwProgram.close()
      pwStrategies.close()
      //      pwValue.close()
    })
  }




}
