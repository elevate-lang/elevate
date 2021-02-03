package elevate.heuristic_search.util

import java.io.{File, FileOutputStream, PrintWriter}
import java.nio.file.{Files, Paths}

import scala.sys.process._
import scala.language.postfixOps
import elevate.core.{RewriteResult, Strategy}

class Path[P](program:P,
              value:Option[Double],
              var initial:PathElement[P] = null,
              var current:PathElement[P] = null
             ){
    // create initial path element
    initial = new PathElement[P](program, null, value, null, null)
    // set initial path element to current element
    current = initial

  def add(program: P, strategy: Strategy[P], value:Option[Double]): Unit = {
    // create new path element
    val elem = new PathElement[P](program, strategy, value, current, null)

    // set new element as successor of current element
    current.successor = elem

    //set new element as current element
    current = elem
  }

  def printPathConsole(): Unit = {
    var tmp = initial
    println("printPath: ")

    while (tmp != null) {
      println("program: " + tmp.program)
      println("strategy: " + tmp.strategy)
      println("value: " + tmp.value)
      tmp = tmp.successor
    }
  }

  def writePathToDot(filename:String) = {
    var tmp = initial

    // prepare file
    var full:String = "graph path {\n"
    var reduced:String = "graph path {\n"

    while (tmp != null) {
      // write to file
      full += "\" "+ Integer.toHexString(tmp.program.hashCode()) + " \" [label = \" " + tmp.program.toString  + "\n" + tmp.value + " \"]; \n"
      reduced += "\" "+ Integer.toHexString(tmp.program.hashCode()) + " \" [label = \" " + Integer.toHexString(tmp.program.hashCode()) + "\n" + tmp.value + " \"]; \n"
      tmp = tmp.successor
    }

    tmp = initial.successor
    while(tmp != null){
      full += "\" "+ Integer.toHexString(tmp.predecessor.program.hashCode()) + " \" -- \" " + Integer.toHexString(tmp.program.hashCode()) + " \"  [label = \" " + tmp.strategy + " \"]; \n"
      reduced += "\" "+ Integer.toHexString(tmp.predecessor.program.hashCode()) + " \" -- \" " + Integer.toHexString(tmp.program.hashCode()) + " \"  [label = \" " + tmp.strategy + " \"]; \n"

      tmp = tmp.successor
    }

    // finish file
    full += "}"
    reduced += "}"

    //check if file exists and avoid overwriting
    val uniqueFilename_full = getUniqueFilename(filename, 4)
    val uniqueFilename_reduced = uniqueFilename_full.substring(0, uniqueFilename_full.length-4)  + "_plain" + ".dot"

    // create new files
    val pwFull = new PrintWriter(new FileOutputStream(new File(uniqueFilename_full), false))
    val pwReduced = new PrintWriter(new FileOutputStream(new File(uniqueFilename_reduced), false))

    // write string encoding path to file
    pwFull.write(full)
    pwReduced.write(reduced)

    // close files
    pwFull.close()
    pwReduced.close()

  }

  def writePathToDisk(filename: String) = {
    // traverse from initial to current

    // write high-level expression and strategy list to files on disk
    var tmp = initial

    while ({ {
      // get unique filename
      val uniqueFilename = getUniqueFilename(filename + "/Expressions/" + Integer.toHexString(tmp.program.hashCode()), 0)
      // create folder
      (s"mkdir ${uniqueFilename}" !!)

      // create file for expression
      val pwProgram = new PrintWriter(new FileOutputStream(new File(uniqueFilename + "/" + Integer.toHexString(tmp.program.hashCode())), false))

      // create file for strategies
      val pwStrategies = new PrintWriter(new FileOutputStream(new File(uniqueFilename + "/strategies"), false))

      // write expression to file
      pwProgram.write(tmp.program.toString)

      // strategy list
     val list = getStrategies(tmp)

      // create strategy string for file
      var strategyString = ""
      list.foreach(elem=>{
        strategyString +=  elem.toString + "\n"
      })

      // write strategy string to file
      pwStrategies.write(strategyString)

      // close files
      pwProgram.close()
      pwStrategies.close()

      tmp = tmp.successor
    } ;tmp != null}) ()

  }

  def getStrategies(element: PathElement[P]):Seq[P=>RewriteResult[P]] = {
    var tmp = initial
    val strategies = scala.collection.mutable.ListBuffer.empty[P=>RewriteResult[P]]

    // there is no first strategy resulting in the initial expression

    // add elements to list (start with second)
    while(tmp != element) {
      tmp = tmp.successor
      strategies += tmp.strategy
    }

    // return sequence
    strategies.toSeq
  }

  def getUniqueFilename(filename:String, offset: Int):String= {
    var uniqueFilename_full = filename

    // check if file or folder already exists
    if(Files.exists(Paths.get(uniqueFilename_full))){
      //val warningString = "Warning! Clash at " + uniqueFilename_full + ".\n"
      //println(warningString + "adding System.currentTimeMillis().")

      // wait for it
      Thread.sleep(1)

      // append timestamp
      val end = uniqueFilename_full.substring(uniqueFilename_full.length-offset, uniqueFilename_full.length)
      uniqueFilename_full = uniqueFilename_full.substring(0, uniqueFilename_full.length-offset)+ "_" + System.currentTimeMillis() + end
    }

    uniqueFilename_full
  }

}

class PathElement[P] (val program:P,
                      val strategy:Strategy[P],
                      val value:Option[Double],
                      var predecessor:PathElement[P],
                      var successor:PathElement[P]
                      ){
  def setSuccessor(elem:PathElement[P]): Unit ={
    successor = elem
  }
}
