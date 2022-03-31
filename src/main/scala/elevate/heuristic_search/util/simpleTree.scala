package elevate.heuristic_search.util
import elevate.core.Strategy

import java.io.{File, FileOutputStream, PrintWriter}
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

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

    val doe = 10
    val optimizationIterations = 100

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
      "scalarization_method": "linear",
      "optimization_method": "opentuner",
      "optimization_iterations" : ${optimizationIterations},
      "input_parameters" : {\n"""

    }


    var entries = ""

    val json = getConstraints()
    json.foreach(elem => {

      val dependency: String = elem._1 == 1 match {
        case true => ""
        case false => "\"" + s"s${elem._1-1}" + "\""
      }

      entries +=
        s"""        "s${elem._1}" : {
           |          "parameter_type" : "ordinal",
           |          "values" : ${SearchSpaceHelper.strategies.map(x => x._2).toSeq.sorted.mkString("[", ", ", "]")},
           |          "constraints" : ${elem._2.mkString("[\"", " | ", "\"]")},
           |          "dependencies" : [${dependency}]
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


  def getConstraints(): mutable.HashMap[Int, Set[String]] = {

    // generateConstraints
    val constraints = mutable.HashMap.empty[Int, Set[String]]
    def generateConstraints(simpleTreeElement: SimpleTreeElement[P]): Unit = {

      val constraint = simpleTreeElement.predecessor.rewrite match {
        case -1 => s"(s${simpleTreeElement.layer.toString}" + " == " + simpleTreeElement.rewrite + ")"
        case _ => s"((s${simpleTreeElement.layer.toString}" + " == " + simpleTreeElement.rewrite + ")" + " & " + s"(s${simpleTreeElement.predecessor.layer.toString}" + " == " + simpleTreeElement.predecessor.rewrite + "))"
      }

      val elem: Set[String] = constraints.isDefinedAt(simpleTreeElement.layer) match {
        case true => constraints.apply(simpleTreeElement.layer)
        case false => Set.empty[String]
      }

      val con: Set[String] = elem ++ (Set(constraint))

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
