package elevate.heuristic_search.util

import java.io.{File, FileOutputStream, PrintWriter}
import java.nio.file.{Files, Paths}
import scala.sys.process._
import scala.language.postfixOps
import elevate.core.{RewriteResult, Strategy}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer


class PathElement[P](override val solution: Solution[P],
                     override val value: Option[Double],
                     val strategy: Strategy[P],
                     var predecessor: PathElement[P],
                     var successor: PathElement[P],
                     val visitNumber: Int
                    ) extends SearchSpaceElement[P](solution, value) {

  def setSuccessor(elem: PathElement[P]): Unit = {
    successor = elem
  }

}

class Path[P](program: P,
              value: Option[Double],
              var initial: PathElement[P] = null,
              var current: PathElement[P] = null,
              var elements: Int
             ) extends SearchSpace[P] {

  elements = 1
  // create initial path element
  initial = new PathElement[P](Solution(program, Seq.empty[Strategy[P]]), value, null, null, null, elements)
  // set initial path element to current element
  current = initial
  val hashmap = mutable.HashMap.empty[String, Solution[P]]
  val visited = mutable.HashMap.empty[String, Solution[P]]

  override def getSize(): Int = {
    hashmap.size
  }

  override def printConsole(): Unit = {
    var tmp = initial
    println("printPath: ")

    while (tmp != null) {
      println("program: " + tmp.solution.expression)
      println("strategy: " + tmp.strategy)
      println("value: " + tmp.value)
      tmp = tmp.successor
    }
  }


  override def getSizeTotal(): Int = {
    var counter = 0
    var tmp = initial

    while (tmp != null) {
      counter += 1
      tmp = tmp.successor
    }

    counter
  }

  // todo think about this
  override def getElement(i: Int): SearchSpaceElement[P] = {
    // check if size of path is exceeded by i

    var counter = 0
    var tmp = initial

    while (tmp != null && counter < i) {
      counter += 1
      tmp = tmp.successor
    }

    tmp.asInstanceOf[SearchSpaceElement[P]]
  }

  // shrink path
  // list all possible solutions without duplicates
  override def getSearchSpace(): Seq[Solution[P]] = {

    // throw all in hashmap
    val hashmap = mutable.HashMap.empty[String, Solution[P]]

    // initialize hashmap
    var tmp = initial
    hashmap += (hashProgram(tmp.solution.expression) -> tmp.solution)

    // iterate over all path nodes
    while (tmp.successor != null) {
      tmp = tmp.successor

      // duplicates?
      //      hashmap.addOne(hashProgram(tmp.program) -> Solution(tmp.program, Seq(tmp.strategy)))
      hashmap += (hashProgram(tmp.solution.expression) -> tmp.solution)
    }

    // convert to Seq
    val seq = hashmap.toSeq.map(elem => elem._2)

    println("path size: " + getSize())
    println("hashmap size: " + hashmap.size)
    println("seq size: " + seq.size)

    seq
  }


  def add(program: P, strategy: Strategy[P], value: Option[Double]): Unit = {
    elements += 1

    // create new path element
    val elem = new PathElement[P](Solution(program, current.solution.strategies :+ strategy), value, strategy, current, null, elements)

    hashmap += (hashProgram(program) -> Solution(program, current.solution.strategies :+ strategy))

    // set new element as successor of current element
    current.successor = elem

    //set new element as current element
    current = elem
  }


  override def writeToDot(filename: String): String = {
    var tmp = initial

    // prepare file
    var full: String = "strict digraph path {\n"
    var full2: String = "strict digraph path {\n"
    var reduced: String = "strict digraph path {\n"
    var reduced2: String = "strict digraph path {\n"
    var reduced3: String = "strict digraph path {\n"
    var reduced4: String = "strict digraph path {\n"

    println("write nodes")
    // write nodes
    var counter = 0
    while (tmp != null) {
      //      println("node: " + counter)
      //      counter += 1
      // for each node traverse tree and match for given hash code
      var tmp2 = initial
      val visitCounter = new ListBuffer[Int]

      // initialize strategy string
      var strategyString = tmp.strategy match {
        case null => ""
        case value => value.toString()
      }

      //      while(tmp2 != null){
      //        // if element was visited later in graph
      //        if(tmp2.program.hashCode() == tmp.program.hashCode()){
      //          visitCounter += tmp2.visitNumber
      //
      //          // count strategies
      //          // todo reduce this to actual strategies
      //          strategyString = tmp2.strategy match {
      //            case null => strategyString
      //            case value => strategyString + "\n" + value.toString()
      //          }
      //
      //        }
      //        tmp2 = tmp2.successor
      //      }


      // todo fix bugs in path!
      // remove all pairs "rule - revert"

      // write strategy, which lead to this node
      // todo list all strategies leading to this node starting from the initial node

      val hash = hashProgram(tmp.solution.expression)

      val value = tmp.value match {
        case Some(runtime) => runtime.toString
        case None => "None"
      }

      full += "\"" + hash + "\" [label = \" " + "[" + visitCounter.toSeq.mkString(",") + "] \n n" + tmp.solution.expression.toString + "\n" + value + "\"]; \n"
      full2 += "\"" + hash + "\" [label = \" " + tmp.solution.expression.toString + "\n" + value + "\"]; \n"

      reduced += "\"" + hash + "\" [label = \"" + "[" + visitCounter.toSeq.mkString(",") + "] \n" + hash + "\\n" + strategyString + "\\n" + value + "\"]; \n"
      reduced2 += "\"" + hash + "\" [label = \"" + "[" + visitCounter.toSeq.mkString(",") + "] \n" + hash + "\\n" + value + "\"]; \n"
      reduced3 += "\"" + hash + "\" [label = \"" + hash + "\\n" + value + "\"]; \n"

      reduced4 += "\"" + hash + "\" [label = \"" + hash.substring(0, 2) + "\\n" + value + "\"]; \n"
      tmp = tmp.successor
    }

    println("write edges")

    // write edges
    tmp = initial.successor
    var appendix = ""
    while (tmp != null) {

      val hash = hashProgram(tmp.solution.expression)
      val hashPredecessor = hashProgram(tmp.predecessor.solution.expression)


      appendix += "\"" + hashPredecessor + "\" -> \"" + hash + "\"  [label = \"" + tmp.strategy + "\"]; \n"


      //      full += "\"" + hashPredecessor + "\" -> \"" + hash + "\"  [label = \"" + tmp.strategy + "\"]; \n"
      //      full2 += "\"" + hashPredecessor + "\" -> \"" + hash + "\"  [label = \"" + tmp.strategy + "\"]; \n"

      //      reduced += "\"" + hashPredecessor + "\" -> \"" + hash + "\"  [label = \"" + tmp.strategy + "\"]; \n"
      //      reduced2 += "\"" + hashPredecessor + "\" -> \"" + hash + "\"  [label = \"" + tmp.strategy + "\"]; \n"
      //      reduced3 += "\"" + hashPredecessor + "\" -> \"" + hash + "\"  [label = \"" + tmp.strategy + "\"]; \n"

      tmp = tmp.successor
    }

    println("reverse order")

    // reverse order linewise
    appendix = appendix.split("\n")
      .toSeq
      .map(_.trim)
      .filter(_ != "").reverse.mkString("\n")

    println("append")

    // append to dot graphs
    full += appendix
    full2 += appendix

    reduced += appendix
    reduced2 += appendix
    reduced3 += appendix
    reduced4 += appendix

    // finish file
    full += "}"
    full2 += "}"
    reduced += "}"
    reduced2 += "}"
    reduced3 += "}"
    reduced4 += "}"

    //check if file exists and avoid overwriting
    val uniqueFilename_full = getUniqueFilename(filename, 4)
    val uniqueFilename_full2 = uniqueFilename_full.substring(0, uniqueFilename_full.length - 4) + "_no_numbers" + ".dot"

    val uniqueFilename_reduced = uniqueFilename_full.substring(0, uniqueFilename_full.length - 4) + "_plain_strategies" + ".dot"
    val uniqueFilename_reduced2 = uniqueFilename_full.substring(0, uniqueFilename_full.length - 4) + "_plain" + ".dot"
    val uniqueFilename_reduced3 = uniqueFilename_full.substring(0, uniqueFilename_full.length - 4) + "_plain_no_numbers" + ".dot"
    val uniqueFilename_reduced4 = uniqueFilename_full.substring(0, uniqueFilename_full.length - 4) + "_plain_no_numbers_reduced" + ".dot"

    // create new files
    val pwFull = new PrintWriter(new FileOutputStream(new File(uniqueFilename_full), false))
    val pwFull2 = new PrintWriter(new FileOutputStream(new File(uniqueFilename_full2), false))

    val pwReduced = new PrintWriter(new FileOutputStream(new File(uniqueFilename_reduced), false))
    val pwReduced2 = new PrintWriter(new FileOutputStream(new File(uniqueFilename_reduced2), false))
    val pwReduced3 = new PrintWriter(new FileOutputStream(new File(uniqueFilename_reduced3), false))
    val pwReduced4 = new PrintWriter(new FileOutputStream(new File(uniqueFilename_reduced4), false))

    // write string encoding path to file
    pwFull.write(full)
    pwFull2.write(full2)

    pwReduced.write(reduced)
    pwReduced2.write(reduced2)
    pwReduced3.write(reduced3)
    pwReduced4.write(reduced4)

    // close files
    pwFull.close()
    pwFull2.close()

    pwReduced.close()
    pwReduced2.close()
    pwReduced3.close()
    pwReduced4.close()


    // visualize dot graph
    (s"dot -Tpng -O " + uniqueFilename_full !!)
    (s"dot -Tpdf -O " + uniqueFilename_full !!)

    (s"dot -Tpdf -O " + uniqueFilename_full2 !!)
    (s"dot -Tpdf -O " + uniqueFilename_full2 !!)

    (s"dot -Tpng -O " + uniqueFilename_reduced !!)
    (s"dot -Tpdf -O " + uniqueFilename_reduced !!)

    (s"dot -Tpng -O " + uniqueFilename_reduced2 !!)
    (s"dot -Tpdf -O " + uniqueFilename_reduced2 !!)

    (s"dot -Tpng -O " + uniqueFilename_reduced3 !!)
    (s"dot -Tpdf -O " + uniqueFilename_reduced3 !!)

    (s"dot -Tpng -O " + uniqueFilename_reduced4 !!)
    (s"dot -Tpdf -O " + uniqueFilename_reduced4 !!)

  }

  // todo same export for hashmap
  def writeAllToDisk(filename: String): Unit = {
    // traverse from initial to current

    // write high-level expression and strategy list to files on disk
    var tmp = initial

    do {
      val hash = hashProgram(tmp.solution.expression)
      // get unique filename
      val uniqueFilename = getUniqueFilename(filename + "/Expressions/" + hash, 0)
      // create folder
      (s"mkdir ${uniqueFilename}" !!)

      // create file for expression
      val pwProgram = new PrintWriter(new FileOutputStream(new File(uniqueFilename + "/" + hash), false))

      // create file for strategies
      val pwStrategies = new PrintWriter(new FileOutputStream(new File(uniqueFilename + "/strategies"), false))

      // write expression to file
      pwProgram.write(tmp.solution.expression.toString)

      // strategy list
      val list = getStrategies(tmp)

      // create strategy string for file
      var strategyString = ""
      list.foreach(elem => {
        elem match {
          case null => strategyString += "null" + "\n"
          case _ => strategyString += elem.toString + "\n"
        }
      })

      // write strategy string to file
      pwStrategies.write(strategyString)

      // close files
      pwProgram.close()
      pwStrategies.close()

      tmp = tmp.successor
    } while (tmp != null)

  }

  // todo think about this? compare to writeToDisk
  override def writeToDisk(filename: String) = {

    hashmap.foreach(elem => {

      // hash program
      //      val hash = hashProgram(solution.expression)
      val hash = elem._1

      // create unique output folder
      val uniqueFilename = getUniqueFilename(filename + "/Expressions/" + hash, 0)
      // create folder
      (s"mkdir ${uniqueFilename}" !!)

      // create file for expression
      val pwProgram = new PrintWriter(new FileOutputStream(new File(uniqueFilename + "/" + hash), false))

      // create file for strategies
      val pwStrategies = new PrintWriter(new FileOutputStream(new File(uniqueFilename + "/strategies"), false))

      // write expression to file
      pwProgram.write(elem._2.expression.toString)

      // strategy list
      val list = elem._2.strategies

      // create strategy string for file
      var strategyString = ""
      list.foreach(elem => {
        elem match {
          case null => strategyString += "null" + "\n"
          case _ => strategyString += elem.toString + "\n"
        }
      })

      // write strategy string to file
      pwStrategies.write(strategyString)

      // close files
      pwProgram.close()
      pwStrategies.close()

    })
  }

  def getStrategies(element: PathElement[P]): Seq[P => RewriteResult[P]] = {
    var tmp = initial
    val strategies = scala.collection.mutable.ListBuffer.empty[P => RewriteResult[P]]

    // there is no first strategy resulting in the initial expression

    // add elements to list (start with second)
    while (tmp != element) {
      tmp = tmp.successor
      strategies += tmp.strategy
    }

    // return sequence
    strategies.toSeq
  }

  def getUniqueFilename(filename: String, offset: Int): String = {
    var uniqueFilename_full = filename

    // check if file or folder already exists
    if (Files.exists(Paths.get(uniqueFilename_full))) {
      //val warningString = "Warning! Clash at " + uniqueFilename_full + ".\n"
      //println(warningString + "adding System.currentTimeMillis().")

      // wait for it
      Thread.sleep(1)

      // append timestamp
      val end = uniqueFilename_full.substring(uniqueFilename_full.length - offset, uniqueFilename_full.length)
      uniqueFilename_full = uniqueFilename_full.substring(0, uniqueFilename_full.length - offset) + "_" + System.currentTimeMillis() + end
    }

    uniqueFilename_full
  }

  def setCurrent(pathElement: PathElement[P]) = {
    current = pathElement
  }

}

