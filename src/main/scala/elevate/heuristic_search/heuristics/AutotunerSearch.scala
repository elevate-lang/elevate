package elevate.heuristic_search.heuristics

import elevate.heuristic_search.util.{Path, PathElement, Solution, hashProgram}
import elevate.heuristic_search.{Heuristic, HeuristicPanel}

import scala.collection.immutable.Queue
import scala.util.Random

class AutotunerSearch[P] extends Heuristic[P] {


  // create search space using exhaustive/breadth first search and store in path

  // then, count nodes in path and give to auto tuner

  // neighbours?

  // evaluate using impl.


  def start(panel:HeuristicPanel[P], initialSolution:Solution[P], depth:Int): (P, Option[Double], Path[P]) = {


    println("depth: " + depth)

    var solution = initialSolution
    var solutionValue = panel.f(solution)
//    val solutionValue = panel.f(solution)

    // craete path
    val path = new Path(solution.expression, solutionValue, null, null, 0)

    var queue = Queue.empty[(Solution[P], PathElement[P])]
    queue = queue.enqueue(solution, path.initial)

    var i = 0
    while(!queue.isEmpty && i < depth) {
      i += 1

      println("i: " + i)
      println("queue size: " + queue.size)
      println("queue: " + queue)

      // get element from queue
      val current = queue.dequeue

      //      println("current: " + current)

      // update current path element
      //      path.setCurrent(current._1._2)
      // todo reach this from start (step by step)
      path.add(current._1._2.program, current._1._2.strategy, current._1._2.value)

      // start at initial node
      var down = path.initial

      println("\n")
      println(" --------- go down ---------- ")
      // go down step by step until reaching current program
      while (hashProgram(current._1._2.program) != hashProgram(down.program)) {
        println("down: " + hashProgram(down.program))
        println("current: " + hashProgram(current._1._2.program))
        down = down.successor
        //        tmp.program.hashCode() == tmp.successor.program.hashCode()){
        // go one step down
        path.add(down.program, down.strategy, down.value)
      }
      println(" --------- finished ---------- ")
      println("\n")


      // update queue
      queue = current._2

      // get neighborhood
      val Ns = panel.N(current._1._1)

      Ns.foreach(ne => {
        //        path.writePathToDot("/home/jo/development/rise-lang/shine/exploration/dot/mv.dot")
        // eval function value

        // add path element
//                val fne = panel.f(ne)
//        path.add(ne.expression, ne.strategies.last, fne)
        path.add(ne.expression, ne.strategies.last, None)

        // add path element and solution to queue
        queue = queue.enqueue((ne, path.current))

        // revert path
        path.add(current._1._1.expression, elevate.core.strategies.basic.revert, current._1._2.value)
      })


      println("\n")
      println(" --------- go up ---------- ")
      var up = current._1._2
      while (up.predecessor != null) {
        up = up.predecessor
        path.add(up.program, elevate.core.strategies.basic.revert, up.value)
      }
      println(" --------- finished ---------- ")
      println("\n")
      //      current._1._2.predecessor match {
      //        case null => // do nothing
      //        case _ =>
      //           go back to parent
      //          path.add(current._1._2.predecessor.program, elevate.core.strategies.basic.revert, current._1._2.predecessor.value)
      //      }

    }

    // now count
    // maybe collaps path
    // doubled things?


//    path.printPathConsole()

    val size = path.getSize()
    println("size: " + size)

    val elem = path.getElement(0)

    println("elem: \n" + elem.program)
    println("elem: \n" + elem.value)

    // random number generator
    val random = new scala.util.Random

    var j = 0
    val max = 100

    println("start random exploration")
    while(j < max){
      val index = random.nextInt(size)
      j += 1

      // get element from index
      val elem = path.getElement(index)
      val tmpSolution = Solution(elem.program, null)

      println("elem: " + elem.program)
      println("execute index: " + index)
      val result = panel.f(tmpSolution)
      println("result: " + result)

      // update solution value if better performance is found
      solutionValue = result match {
        case Some(value) => {
          value <= solutionValue.get  match {
            case true =>
              println("better")
              // update solution
              solution = tmpSolution
              // return result as new solution value
              result
            case false =>
              println("not better")
              solutionValue
          }
        }
        case None => solutionValue
      }
    }

    println("end")
    println("solution: " + solution.expression)
    println("solutionValue: " + solutionValue)

    // generate stuff
    // start tuner

    // last?
    (solution.expression, solutionValue, path)
  }

}
