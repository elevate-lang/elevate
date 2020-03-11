package elevate.heuristic_search.util

import java.io.{File, FileOutputStream, PrintWriter}

import elevate.core.Strategy

class Path[P](program:P,
              value:Double,
              var initial:PathElement[P] = null,
              var current:PathElement[P] = null
             ){
    // create initial path element
    initial = new PathElement[P](program, null, value, null, null)
    // set initial path element to current element
    current = initial

  def add(program: P, strategy: Strategy[P], value:Double): Unit = {
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

  def writePathToDot() = {
    var tmp = initial

    // prepare file
    var full:String = "graph path {\n"
    var reduced:String = "graph path {\n"

    while (tmp != null) {
      // write to file
      full += "\" "+ tmp.program.hashCode() + " \" [label = \" " + tmp.program.toString  + " \"]; \n"
      reduced += "\" "+ tmp.program.hashCode() + " \" [label = \" " + tmp.program.hashCode()  + " \"]; \n"
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


    // print String to file
    // remove hard coded paths
    val pwFull = new PrintWriter(new FileOutputStream(new File("/home/jo/developement/rise-lang/exploration/path_full.dot"), false))
    val pwReduced = new PrintWriter(new FileOutputStream(new File("/home/jo/developement/rise-lang/exploration/path_reduced.dot"), false))

    pwFull.write(full)
    pwReduced.write(reduced)

    pwFull.close()
    pwReduced.close()

  }
}

class PathElement[P] (val program:P,
                      val strategy:Strategy[P],
                      val value:Double,
                      var predecessor:PathElement[P],
                      var successor:PathElement[P]
                      ){
  def setSuccessor(elem:PathElement[P]): Unit ={
    successor = elem
  }
}
