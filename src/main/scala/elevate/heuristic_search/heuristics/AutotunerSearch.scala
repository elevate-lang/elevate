package elevate.heuristic_search.heuristics

import elevate.heuristic_search.util.{Path, Solution, hashProgram}
import elevate.heuristic_search.{Heuristic, HeuristicPanel}
import jdk.jfr.Timespan

import scala.collection.immutable.Queue
import scala.collection.mutable
import scala.util.Random

class AutotunerSearch[P] extends Heuristic[P] {

  def start(panel:HeuristicPanel[P], initialSolution:Solution[P], depth:Int): (P, Option[Double], Path[P]) = {

    println("depth: " + depth)

    val totalDurationStart = System.currentTimeMillis()

    var solution = initialSolution
//    var solutionValue = panel.f(solution)
    var solutionValue: Option[Double] = Some(1000.toDouble)

    // create path, queue and hashmap
    val path = new Path(solution.expression, solutionValue, null, null, 0) // still necessary?
    var queue = Queue.empty[(Int, Solution[P])]
    val hashmap = mutable.HashMap.empty[String, Solution[P]]

    queue = queue.enqueue((0, solution))
    hashmap += (hashProgram(solution.expression) -> solution)


    // parallel and synchronized
    def dq():(Int, (Int, Solution[P])) = this.synchronized {

        val current = queue.dequeue

        // update queue
        queue = current._2
        val layer = current._1._1 + 1

      (layer, current._1)
    }

    def enq(layer: Int, Ns: Set[Solution[P]]) = this.synchronized {
      // add elements
      // add elements from neighborhood to queue
      Ns.foreach(ne => {
        // if last layer is reached don't enqueue
        if (layer < depth) {
          queue = queue.enqueue((layer, ne))
        }

        // add to hashmap, if not present yet
        val hash = hashProgram(ne.expression)
        hashmap.get(hash) match {
          case Some(_) => // do nothing
          case None => hashmap += (hash -> ne)
        }
      })
    }

    // parallel
    def grow(current: (Int, Solution[P])): Set[Solution[P]] = {
      // get neighborhood of current solution
      panel.N(current._2)
    }

    if (depth > 0) {

      var i = 0
      var old_layer = 0
      while (!queue.isEmpty) {
        i += 1

        println("iteration: " + i )
//        println("nodes: " + hashmap.size)
        println("queue size: " + queue.size)

//        val current = queue.dequeue
//
//        // update queue
//        queue = current._2
//        val layer = current._1._1 + 1
//
//        if (layer > old_layer){
//          old_layer = layer
//          println("layer: " + layer)
//        }


        // determine thread numbers
        var threads = 1
        if (queue.size >= 8) {
          threads = 8
        }

        val work = queue.size/threads
        println("threads: " + threads)
        println("work per thread: "  + work)

        val threadList = mutable.ListBuffer.empty[Thread]

        // maybe use thread pool
        for (i <- 1 to threads) {
//          println("thread i: " + i)
          val thread = new Thread {
            override def run: Unit = {

              // do work on thread
              for (j <- 1 to work) {

                val (layer, current) = this.synchronized {
                  dq()
                }

//                val Ns = this.synchronized {
//                  grow(current) // rewrite parallel
//                }
                val Ns = grow(current)

                this.synchronized {
                  enq(layer, Ns) // add elements synchorniedz
                }
              }
            }
          }
          thread.start
          threadList.addOne(thread)

          //          Thread.sleep(10) // slow the loop down a bit
        }

//        println("finish")
        threadList.foreach(thread => thread.join())

        // get neighborhood of current solution
//        val Ns = panel.N(current._1._2)

        // add elements from neighborhood to queue
//        Ns.foreach(ne => {
////           if last layer is reached don't enqueue
//          if (layer < depth) {
//            queue = queue.enqueue((layer, ne))
//          }
//
//          // add to hashmap, if not present yet
//          val hash = hashProgram(ne.expression)
//          hashmap.get(hash) match {
//            case Some(_) => // do nothing
//            case None => hashmap += (hash -> ne)
//          }
//        })
      }
    }

//
//    if (depth > 0) {
//
//      var i = 0
//      var old_layer = 0
//      while (!queue.isEmpty) {
//        i += 1
//
//        //        println("node iteration: " + i + "/" + depth)
//        //        println("nodes: " + hashmap.size)
//        //        println("queue size: " + queue.size)
//
//                val current = queue.dequeue
//        //
//                // update queue
//                queue = current._2
//                val layer = current._1._1 + 1
//        //
//                if (layer > old_layer){
//                  old_layer = layer
//                  println("layer: " + layer)
//                }
//
////         get neighborhood of current solution
//                val Ns = panel.N(current._1._2)
//
//        // add elements from neighborhood to queue
//                Ns.foreach(ne => {
//        //           if last layer is reached don't enqueue
//                  if (layer < depth) {
//                    queue = queue.enqueue((layer, ne))
//                  }
//
//                  // add to hashmap, if not present yet
//                  val hash = hashProgram(ne.expression)
//                  hashmap.get(hash) match {
//                    case Some(_) => // do nothing
//                    case None => hashmap += (hash -> ne)
//                  }
//                })
//      }
//    }


    // todo config file generation -> duplicates in constraints
    // check generation of constraints

    val searchSpace = hashmap.toSeq.map(elem => elem._2)
    val size = searchSpace.size

    println("search space: " + searchSpace.size)
    println("hashmap size: " + hashmap.size)

    val duration:Double = (System.currentTimeMillis() - totalDurationStart).toDouble



    println("duration: " + (duration) + "ms")
    println("duration: " + (duration/1000) + "s")
    println("duration: " +  (duration/1000/60) + "m")

    // random number generator
    val random = new scala.util.Random

    var j = 0
    val max = 10000

    println("start random exploration")
    val explorationStartingPoint = System.currentTimeMillis()
    while(j < max){
      val index = random.nextInt(size)
      j += 1

      val candidate = searchSpace.apply(index)

//      println("elem: " + candidate.expression)
//      println("execute index: " + index)
      val result = panel.f(candidate)
//      println("result: " + result)

      // update solution value if better performance is found
      solutionValue = result match {
        case Some(value) => {
          value <= solutionValue.get  match {
            case true =>
//              println("better")
              // update solution
              solution = candidate
              // return result as new solution value
              result
            case false =>
//              println("not better")
              solutionValue
          }
        }
        case None => solutionValue
      }
    }

    val duration2 = (System.currentTimeMillis() - explorationStartingPoint).toDouble
    println("duration2: " + duration2/1000  + "s")

    // todo write all tested elements to disk
//    path.writePathToDisk()

    // write solutions to disk

    println("end")
    println("solution: " + solution.expression)
    println("solutionValue: " + solutionValue)
    println("strategies: ")
    solution.strategies.foreach(println)

    // generate stuff
    // start tuner

    // last?
    (solution.expression, solutionValue, path)
  }

}
