package elevate.heuristic_search.heuristics

import elevate.heuristic_search.util.{Path, Solution, hashProgram, hashSolution}
import elevate.heuristic_search._

import scala.collection.mutable

class Random[P] extends Heuristic[P] {
  // initialize global best
  var best: Option[Double] = None


  def start(panel: HeuristicPanel[P], initialSolution: Solution[P], depth: Int, samples: Int): ExplorationResult[P] = {
    var solution = initialSolution
    //    var solution = new Solution[P](initialSolution, scala.collection.mutable.Seq.empty[Strategy[P]]

    //    val path = new Path(solution.expression, panel.f(solution), null, null, 0)
    val random = scala.util.Random

    // iterations in total
    val maxDepth = depth

    // how many rounds
    val rounds = samples / depth

    // backtracking?


    // avoid duplicates
    // hashmap?
    //    val visited = mutable.HashSet.empty[String]
    //    val visited = mutable.HashSet.empty[Seq[Int]]

    // add starting expression to elems?
    // check if we need to hash strategies as well
    //    visited.add(Seq.empty[Int])

    var visited = mutable.HashMap.empty[Int, mutable.HashSet[Seq[Int]]]
    var executed = mutable.HashSet.empty[Seq[Int]]

    // add empty foreach layer
    Range(0, depth + 1).foreach(layer => visited.addOne(layer, mutable.HashSet.empty[Seq[Int]]))

    //    val test2 = visited.map(elem => elem._2.size).reduceLeft((a,b) => a+b)


    // todo make it easy -> just check naive
    // might run into performance issues but we can improve on this later
    //    def getValidCandidates(rewritesSoFar: Seq[Int], candidatesSize: Int): Seq[Int] = {
    //      val candidates = Range(0, candidatesSize)
    //
    //      // get visited rewrites for this layer
    //      val visitedLayer = visited.get(rewritesSoFar.size + 1)
    //
    //      val candidatesFiltered: Seq[Int] = visitedLayer match {
    //        case Some(value) => candidates.filter(cand => !value.contains(rewritesSoFar :+ cand))
    //        case None => candidates // all are valid
    //      }
    //
    //      // if just one left add to hashmap
    // after this we won't have any valid element left

    // todo add this later
    //
    //      if (candidatesFiltered.size == 1 && visited.get(rewritesSoFar.size + 1).isEmpty
    //
    //      if (candidatesFiltered.size == 1 && rewritesSoFar.size == depth) {
    //        visited.apply(rewritesSoFar.size - 1) = visited.get(rewritesSoFar.size -1 ).get.add(rewritesSoFar)
    //      }
    //
    //      if (candidatesFiltered.size == 0) {
    //        throw new Exception("candidates zero - something went wrong")
    //      }
    //
    //      // if candidatesFiltered.size == 0
    //      // add this to hashmap
    //
    //      candidatesFiltered
    //    }


    var counter = 0
    while (counter < samples) {
      //      println("prepare ")
      //    }

      //    for (i <- Range(0, rounds)) {
      //      println(s"Iteration: [${i}]")
      // repeat
      //      println("solution: " + hashProgram(solution.expression))
      //      println("strategos: " + solution.strategies.size)
      //      solution.strategies.foreach(println)
      //
      //      println("get random number ")
      //      val depth = random.nextInt(maxDepth - 1) // make sure we don't have zero
      //      val depth = maxDepth
      //      println(s"[${i}] : depth: ${depth}")

      // rewrite first store in seq

      // just store the random numbers!

      // mutable Seq to store path

      var candidates = Seq.empty[Solution[P]]
      var candidateRewrites = Seq.empty[Int]

      var path = Seq.empty[(Solution[P], Seq[Int])]

      // add initial node to path
      path = path :+ (initialSolution, Seq.empty[Int])

      // get path down the road
      //      for (k <- Range(0, depth + 1)) {

      var k = 0
      while (path.size <= depth) {
        //        println(s"Rewrite: [${k}]")
        //        path.size match {
        //          case 0 => println("empty path")
        //          case _ => println("path: " + path.last._2.mkString("[", ", ", "]"))
        //        }

        //        println("path.size: " + path.size)
        //        println("filter: ")
        //        visited.foreach(elem => {
        //          println("layer: " + elem._1)
        //          elem._2.foreach(elem2 => println(elem2.mkString("[", ", ", "]")))
        //        })

        // rewrite and filter out expressions we have already seen

        // we are not allowed to filter out unless whole subtree is explored completely

        // if last rewrite results in invalid backtrack
        // todo check indices

        // get neighrbours
        val Ns = panel.N(path.last._1)

        // get filter for layer k + 1

        val visitedLayer = visited.get(k + 1).get

        val sequence = path.size match {
          case 0 => Seq.empty[Int]
          case _ => path.last._2
        }

        // generate candidates and filter them out
        val candidates: Seq[Seq[Int]] = Range(0, Ns.size).map(elem => sequence :+ elem).filter(elem => !visitedLayer.contains(elem))

        //        println("candidates : " + candidates.mkString("[", ", ", "]"))

        // check if no valid candidate exists
        if (path.size == 1 && candidates.size == 0) {
          // stop we are finished

          return ExplorationResult(
            solution,
            best,
            None
          )
        }

        // if no candidate mark this as dead end and repeat
        //        println("candidates: " + candidates.mkString("[", ", ", "]"))
        if (candidates.size == 0) {
          //          println("subtree is full")
          //          // add path.last._2 to visited layer k
          //          //          visited.apply(k) = visited.get(k).get.add(path.last._2)
          //          // todo check this
          //          println("adde sequecne: " + sequence.mkString("[", ", ", "]"))
          //          println("k: " + k)


          visited.get(k).get.add(sequence)
          //           backtrack
          // drop last element
          //          println("path: " + path.last._2.mkString("[", ", ", "]"))


          //          path.size match {
          //            case 0 =>
          //              p
          //            case _ =>
          //              println("path: " + path.last._2.mkString("[", ", ", "]"))
          //          }


          //          path.size match {
          //            case 0 =>
          //            //              println("path is: empty")
          //            case _ =>
          //            //              println("path is: " + path.last._2.mkString("[", ", ", "]"))
          //          }
          //          println("drop last element and repeat")
          path = path.dropRight(1)

          k = k - 1

          // subtree is full

          //          println("filter: ")
          //          visited.foreach(elem => {
          //            println("layer: " + elem._1)
          //            elem._2.foreach(elem2 => println(elem2.mkString("[", ", ", "]")))
          //          })
          //
          //          System.exit(0)
        } else {
          k += 1
          // random number
          // run
          val randomIndex = random.nextInt(candidates.size)
          //          println("random number is: " + randomIndex)

          // translate random index to
          val rewriteIndex = candidates.apply(randomIndex).last
          //          index.last

          //          val result = Ns(rewriteIndex)
          solution = Ns(rewriteIndex)

          // update path
          path = path :+ (solution, sequence :+ rewriteIndex)
          // update filter
          // don't update filter unless we are at the bottom
          if (k == depth) {
            //            println("filter size total: " + visited.map(elem => elem._2.size).reduceLeft((a, b) => a + b))
            //            println("added !")
            //            println("contains: " + visited.get(k).contains(sequence :+ randomIndex))

            if (visited.get(k).get.contains(sequence :+ rewriteIndex)) {
              throw new Exception("elem is already in filter")
            }

            visited.get(k).get.add(sequence :+ rewriteIndex)
            //            println("filter size total: " + visited.map(elem => elem._2.size).reduceLeft((a, b) => a + b))
            //            visited.addOne((k + 1, visited.get(k + 1).get.add(path.last._2 :+ randomIndex)))
          }
          //          visited.add
        }
        path.size match {
          case 0 =>
          //            println("path is: empty")
          case _ =>
          //            println("path is: " + path.last._2.mkString("[", ", ", "]"))
        }
      }

      //      println("prepare finished \n \n")

      //      path.size match {
      //        case 0 => println("empty path")
      //        case _ => println("path: " + path.last._2.mkString("[", ", ", "]"))
      //      }
      //      println("path.size: " + path.size)
      //      println("filter: ")
      //      visited.foreach(elem => {
      //        println("layer: " + elem._1)
      //        println(elem._2.map(elem2 => elem2.mkString("[", ", ", "]")).toSeq.sorted.mkString("\n"))
      //      })


      //      println("now execute path ")
      //      path.foreach(elem => println(elem._2.mkString("[", ", ", "]")))

      //      println("filter size total: " + visited.map(elem => elem._2).flatten.size)

      //      println("filter of all elements: " + visited.map(elem => elem._2.map(a => a.size)).reduceLeft((a, b) => a.size + b.size))
      //      println("filter last layer: " + visited.get(depth).get.size)
      //      println("visited: " + executed.size)
      var hasExecuted = false
      path.foreach(elem => {

        // at least last layer should be a new expression

        executed.contains(elem._2) match {
          case true => // don't execute and dont' count

            // why?

            // check if it is in filter
            elem._2.size match {
              case `depth` =>
                val filter = visited.apply(elem._2.size)
                //                println("elem: " + elem)
                filter.contains(elem._2) match {
                  case true => throw new Exception("is in filter should not be here")
                  case false => throw new Exception("why it is not filtered out?")
                }
              case _ => // it's okay to no execute if we are not on last layer
            }

          //            }
          case false =>
            //            println(s"[${counter}/${samples}] : runtime")
            //            println(s"[${executed.size}/${samples}: runtime")
            counter += 1

            // add to filter
            //            elem._2.size match {
            //              case `depth` =>
            //                println("filter size total: " + visited.map(elem => elem._2.size).reduceLeft((a, b) => a + b))
            //                visited.get(depth).get.add(elem._2)
            //                println("added to filter!!")
            //                println("filter size total: " + visited.map(elem => elem._2.size).reduceLeft((a, b) => a + b))
            //              case _ =>
            //            }

            hasExecuted = true

            executed.add(elem._2)
            val result = panel.f(elem._1)
        }
      })

      if (hasExecuted == false) {
        throw new Exception("should always execute something")
      }

      //      println("has executed: " + hasExecuted)

      // repeat
      solution = initialSolution
      path = Seq.empty[(Solution[P], Seq[Int])]

    }

    // reset solution
    // todo maybe keep track of global solution
    //      solution = initialSolution
    //      println("finished \n")
    //    }

    ExplorationResult(
      solution,
      best,
      None
    )
  }

}
//
//          panel.f(result) match {
//            case Some(value) =>
////              valid = true
//              //              val oldSolution = solution
//              //              println(value)
//              solution = result
//              //add to path
//              //              println("path.add")
//              //              path.add(solution, Some(value))
//              //              println("path.add finished")
//
//              //              println("check best")
//              // check if new global best is found
//              best match {
//                case None => best = Some(value)
//                case Some(_) =>
//                  value < best.get match {
//                    case true => best = Some(value)
//                    case false =>
//                  }
//              }
//            //              println("check best finished")
//            case None =>
//
//              // todo check this
////              valid = true
//              solution = result
//
//            //              println("None")
//            //              path.add(result, None) // why do we add this here?
//            //              path.add(Solution(current.solution.expression, current.solution.strategies ++ Seq(elevate.core.strategies.basic.revert)), current.value) // and why this?
//          }
//
//        val numbers = getValidCandidates(candidateRewrites, Ns.size)
//
//        // execute?
//
//
//        // get random number from numbers
//
//        // add this to candidates
//
//        // now execute all elements?
//
//
//        //        println("Neighbourhood size: " + Ns_unfiltered.size)
//        //        val Ns = Ns_unfiltered.filter(elem => !visited.contains(hashProgram(elem)))
//        //        println("Neighbourhood size: " + Ns.size)
//        //
//
//        val size = Ns.size
//        var valid = false
//        var j = 0

//        Ns.foreach(elem => {
//          println("solution: " + hashSolution(elem) + "  --  " + elem.strategies.mkString("[", ", ", "]"))
//        })

// visited = HashSet(Seq(0, 0, 0, 0), Seq(0, 0, 0, 1), Seq(0, 0, 0, 2))

// get valid sequence 4
// get valid sequence 3
// get valid sequence 2
// get valid sequence 1

// candidate
// Seq[0, 0, 0, ?]
// check for valid
// valid.size == 0

// go up
// Seq[0, 0, ?, ?]
// try and see


// if layer is full add
// if chosen candidate add it to things

// we only get valid ones but we have to check
//        def getNext(rewrites: Seq[Int]): Boolean = {
//          if (rewrites.size == depth) {
//            visited.contains(rewrites)
//          } else {
//            var randomNumber = 0
//            do {
//              randomNumber = 10
//            } while (getNext(rewrites :+ randomNumber))
//          }
//        }

//          getNext(rewrites :+ randomNumber) match {
//            case true =>
//            case false =>
//          }
///
//           choose one
//          val options = getNext(rewrites, index)
//

// if we are at the end
// backtrack?

// get valids

// if valid = 0
// remove last

//          var index = 0
//          while (true) {
//            valid.size match {
//              case 0 => // no valid elements, let's back track
//               index += 1
//              case _ => // ready, choose one
//            }
//

//}

//          0
//        }
//
//
//        while (!valid && j < size) {
//          //          println("loop started")
//          //          println(s"j: ${j}")
//          j = j + 1
//          //          println("throw dice")
//          val randomIndex = random.nextInt(size)
//
//          // backtrack - recursion?
//
//          //          println("throw dice finished")
//
//          //          println("get result from Ns")
//          val result = Ns.toSeq(randomIndex)
//
//          // add to candidate
//          //          candidate = candidate :+ result
//
//          //          println("got result from Ns")
//
//          //        val oldSolution = solution
//          //        val oldSolutionValue = solutionValue
//          //        solution = result
//
//          //          val current = path.current
//
//          //          println("execute ... ")
//          panel.f(result) match {
//            case Some(value) =>
//              valid = true
//              //              val oldSolution = solution
//              //              println(value)
//              solution = result
//              //add to path
//              //              println("path.add")
//              //              path.add(solution, Some(value))
//              //              println("path.add finished")
//
//              //              println("check best")
//              // check if new global best is found
//              best match {
//                case None => best = Some(value)
//                case Some(_) =>
//                  value < best.get match {
//                    case true => best = Some(value)
//                    case false =>
//                  }
//              }
//            //              println("check best finished")
//            case None =>
//
//              // todo check this
//              valid = true
//              solution = result
//
//
//            //              println("None")
//            //              path.add(result, None) // why do we add this here?
//            //              path.add(Solution(current.solution.expression, current.solution.strategies ++ Seq(elevate.core.strategies.basic.revert)), current.value) // and why this?
//          }
//          //          println("loop finished")
//        }
//      }

//      print("backtrack ...")
// backtrack to initial
//      var up = path.current
//      while (up.predecessor != null) {
//        up = up.predecessor
//        path.add(Solution(up.solution.expression, up.solution.strategies ++ Seq(elevate.core.strategies.basic.revert)), up.value)
//      }
