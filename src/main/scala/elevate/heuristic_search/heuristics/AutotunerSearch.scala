package elevate.heuristic_search.heuristics

import elevate.heuristic_search.util.{Path, PathElement, Solution, hashProgram}
import elevate.heuristic_search.{Heuristic, HeuristicPanel}

import scala.collection.immutable.Queue
import scala.collection.mutable
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
    var queue2 = Queue.empty[Solution[P]]

    queue = queue.enqueue(solution, path.initial)
    queue2 = queue2.enqueue(solution)

    // throw all in hashmap
    val hashmap = mutable.HashMap.empty[String, Solution[P]]
    val hashmap2 = mutable.HashMap.empty[String, Solution[P]]

    // initialize hashmap
//    var tmp = path.initial

//    var tmp = path.initial

    // add initial solution to hashmap
    hashmap += (hashProgram(solution.expression) -> solution)
    hashmap2 += (hashProgram(solution.expression) -> solution)


    // initialize hashmap
//    var tmp = initial
//    hashmap += (hashProgram(tmp.program) -> Solution(tmp.program, Seq(tmp.strategy)))




    var counter2 = 0
    var counter3 = 0
    var i = 0
    while(!queue.isEmpty && i < depth) {
      i += 1

      println("i: " + i)
      println("queue size: " + queue.size)
//      println("queue: " + queue)

      // get element from queue
      val current = queue.dequeue
      val current2 = queue2.dequeue


      val hash = hashProgram(current2._1.expression)
      val hash2 = hashProgram(current._1._1.expression)

      println("hash: " + hash)
      println("hash2: " + hash2)

      // add to hashmap, if not present yet
      hashmap.get(hash) match {
        case Some(solution) =>  println("already there") // do nothing (already present)
        case None => {
          println("not there")
          counter2 += 1

          hashmap += (hash -> current2._1)
        } // add to hashmap
      }

      // add to hashmap, if not present yet
      hashmap2.get(hash2) match {
        case Some(solution) =>  println("already there") // do nothing (already present)
        case None => {
          println("not there")
          counter3 += 1

          hashmap2 += (hash2 -> current._1._1)
        } // add to hashmap
      }

      //      hashmap += (hashProgram(current2._1.expression) -> Solution(current._1._2.program, current._1._1.strategies))

      //      println("current: " + current)

      // update current path element
      //      path.setCurrent(current._1._2)
      // todo reach this from start (step by step)
      // first go down from beginning?
//      path.add(current._1._2.program, current._1._2.strategy, current._1._2.value)

      // start at initial node
      var down = path.initial

      println("\n")
      println(" --------- go down ---------- ")
      // go down step by step until reaching current program
      while (hashProgram(current._1._2.program) != hashProgram(down.program)) {
//        println("down: " + hashProgram(down.program))
//        println("current: " + hashProgram(current._1._2.program))
        down = down.successor
        //        tmp.program.hashCode() == tmp.successor.program.hashCode()){
        // go one step down
        path.add(down.program, down.strategy, down.value)
      }
      println(" --------- finished ---------- ")
      println("\n")


      // update queue
      queue = current._2
      queue2 = current2._2

      val Ns2 = panel.N(current2._1)
      // add elements from neighborhood to queue
      Ns2.foreach(ne => {
        queue2 = queue2.enqueue(ne)

        val hash = hashProgram(ne.expression)

        println("hash: " + hash)

        // add to hashmap, if not present yet
        hashmap.get(hash) match {
          case Some(solution) =>  println("already there") // do nothing (already present)
          case None => {
            println("not there")
            counter2 += 1

            hashmap += (hash -> ne)
          } // add to hashmap
        }


        // add current not neighbors


//        hashmap += (hash -> current2._1)
        // add to hashmap
      })

      // get neighborhood
      val Ns = panel.N(current._1._1)

      Ns.foreach(ne => {
        //        path.writePathToDot("/home/jo/development/rise-lang/shine/exploration/dot/mv.dot")
        // eval function value

        // add path element
//                val fne = panel.f(ne)
//        path.add(ne.expression, ne.strategies.last, fne)
        path.add(ne.expression, ne.strategies.last, None)

        val hash2 = hashProgram(current._1._1.expression)
        // add to hashmap, if not present yet
        hashmap2.get(hash2) match {
          case Some(solution) =>  println("already there") // do nothing (already present)
          case None => {
            println("not there")
            counter3 += 1

            hashmap2 += (hash2 -> ne.)
          } // add to hashmap
        }

        // add path element and solution to queue
        queue = queue.enqueue((ne, path.current))

        // revert path
        path.add(current._1._1.expression, elevate.core.strategies.basic.revert, current._1._2.value)

        hashmap2.get(hash2) match {
          case Some(solution) =>  println("already there") // do nothing (already present)
          case None => {
            println("not there")
            counter3 += 1

            hashmap2 += (hash2 -> current._1._1)
          } // add to hashmap
        }


      })


      println("\n")
      // go back to parent?
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

//    val size = path.getSize()
//    println("size: " + size)

    val searchSpace = path.getSearchSpace()
    val searchSpace2 = hashmap.toSeq.map(elem => elem._2)


    val size = searchSpace.size
    println("path size: " + path.getSize())
    println("search space: " + size)
    println("search space2: " + searchSpace2.size)
    println("hashmap size: " + hashmap.size)
    println("counter2 (elements added to hashmap: " + counter2)
//    println("search space: " + searchSpace2)

//    searchSpace2.foreach(elem => {
//      println("elem: \n " + elem)
//    })
//
//    searchSpace.foreach(elem => {
//      println("elem: \n" + elem)
//    })

//    System.exit(0)

//    val elem = path.getElement(0)

//    println("elem: \n" + elem.program)
//    println("elem: \n" + elem.value)

    // random number generator
    val random = new scala.util.Random

    var j = 0
    val max = 0

    println("start random exploration")
    while(j < max){
      val index = random.nextInt(size)
      j += 1

      // get element from index
//      val elem = path.getElement(index)

      val candidate = searchSpace.apply(index)

//      val tmpSolution = Solution(elem.program, null)

      println("elem: " + candidate.expression)
      println("execute index: " + index)
      val result = panel.f(candidate)
      println("result: " + result)

      // update solution value if better performance is found
      solutionValue = result match {
        case Some(value) => {
          value <= solutionValue.get  match {
            case true =>
              println("better")
              // update solution
              solution = candidate
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
