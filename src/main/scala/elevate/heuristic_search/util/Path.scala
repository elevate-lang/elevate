package elevate.heuristic_search.util

import java.io.{File, FileOutputStream, PrintWriter}
import java.nio.file.{Files, Paths}

import elevate.core.Strategy

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
      full += "\" "+ tmp.program.hashCode() + " \" [label = \" " + tmp.program.toString  + "\n" + tmp.value + " \"]; \n"
      reduced += "\" "+ tmp.program.hashCode() + " \" [label = \" " + tmp.program.hashCode() + "\n" + tmp.value + " \"]; \n"
      tmp = tmp.successor
    }

    tmp = initial.successor
    while(tmp != null){
      full += "\" "+ tmp.predecessor.program.hashCode() + " \" -- \" " + tmp.program.hashCode() + " \"  [label = \" " + tmp.strategy + " \"]; \n"
      reduced += "\" "+ tmp.predecessor.program.hashCode() + " \" -- \" " + tmp.program.hashCode() + " \"  [label = \" " + tmp.strategy + " \"]; \n"

      tmp = tmp.successor
    }

    // finish file
    full += "}"
    reduced += "}"

    //check if file exists and avoid overwriting
    var uniqueFilename_full = filename
    var uniqueFilename_reduced = uniqueFilename_full.substring(0, uniqueFilename_full.length-4)  + "_plain" + ".dot"

    if(Files.exists(Paths.get(uniqueFilename_full))){
      val warningString = "Warning! Clash at " + uniqueFilename_full + ".\n"
      println(warningString + "adding System.currentTimeMillis().")
      uniqueFilename_full = uniqueFilename_full.substring(0, uniqueFilename_full.length-4)+ "_" + System.currentTimeMillis() + ".dot"
    }

    if(Files.exists(Paths.get(uniqueFilename_reduced))){
      val warningString = "Warning! Clash at " + uniqueFilename_reduced + ".\n"
      println(warningString + "adding System.currentTimeMillis().")
      uniqueFilename_reduced = uniqueFilename_reduced.substring(0, uniqueFilename_reduced.length-4)+ "_" + System.currentTimeMillis() + ".dot"
    }

    // print String to file
    val pwFull = new PrintWriter(new FileOutputStream(new File(uniqueFilename_full), false))
    val pwReduced = new PrintWriter(new FileOutputStream(new File(uniqueFilename_reduced), false))

    pwFull.write(full)
    pwReduced.write(reduced)

    pwFull.close()
    pwReduced.close()

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
