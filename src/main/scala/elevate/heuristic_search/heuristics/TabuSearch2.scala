package elevate.heuristic_search.heuristics

import elevate.heuristic_search._
import elevate.heuristic_search.util.Solution

import scala.collection.mutable

class TabuSearch2[P] extends Heuristic[P] {
  // initialize global best
  var best: Option[Double] = None


  def start(panel: HeuristicPanel[P], initialSolution: Solution[P], depth: Int, samples: Int): ExplorationResult[P] = {
    var solution = initialSolution

    val random = scala.util.Random

    var visited = mutable.HashMap.empty[Int, mutable.HashSet[Seq[Int]]]
    var executed = mutable.HashSet.empty[Seq[Int]]

    // add empty foreach layer
    Range(0, depth + 1).foreach(layer => visited.addOne(layer, mutable.HashSet.empty[Seq[Int]]))

    val tabuList = scala.collection.mutable.Queue.empty[Seq[Int]]
    val tabuListSize = 100

    var counter = 0

    while (counter < samples) {

      // new path for this round
      var path = Seq.empty[(Solution[P], Seq[Int])]
      path = path :+ (initialSolution, Seq.empty[Int])

      // assemble path
      var k = 0
      while (path.size <= depth) {

        // get neighbours
        val Ns = panel.N(path.last._1)

        // get filter for layer k + 1
        val layerFilter = visited(k + 1)

        // get rewrite sequence so far
        val sequence = path.size match {
          case 0 => Seq.empty[Int]
          case _ => path.last._2
        }

        // generate candidates and filter them out
        val candidates: Seq[Seq[Int]] = Range(0, Ns.size).map(elem => sequence :+ elem).filter(elem => !layerFilter.contains(elem))

        // check if globally no valid candidate exists
        if (path.size == 1 && candidates.isEmpty) {
          return ExplorationResult(
            solution,
            best,
            None
          )
        }

        // if no candidate mark this subtree as fully covered and repeat
        if (candidates.isEmpty) {
          // add subtree to filter
          visited(k).add(sequence)

          // drop subtree from path (sequence)
          path = path.dropRight(1)

          // go step back
          k = k - 1

        } else {

          // if we have a valid candidate choose one randomly and continue
          k += 1


          // todo implement tabu search here

          // apply tabu list to NS

          // apply aspiration list to NS
          // eval Ns
          //          var best =
          candidates.foreach(elem => {
            // execute (check with execution list

            // elem
            val s_mark = Ns(elem.last)

            // execute

            // get minimum of all


          })

          // get best (compare with global best)

          // best is new solution
          // update tabu list
          // update aspiration (parent)
          // are they injected properly?


          // get random number and translate random index to rewrite index
          val randomIndex = random.nextInt(candidates.size)
          val rewriteIndex = candidates.apply(randomIndex).last
          solution = Ns(rewriteIndex)

          // we don't need the path here
          path = path :+ (solution, sequence :+ rewriteIndex)

          // update filter if we are at the bottom
          if (k == depth) {

            // sanity check
            // we should not visit an element which is already in filter
            if (visited(k).contains(sequence :+ rewriteIndex)) {
              throw new Exception("elem is already in filter")
            }

            visited(k).add(sequence :+ rewriteIndex)
          }
        }
      }

      // todo move exeuction to main loop
      // todo check executed counter
      var hasExecuted = false
      path.foreach(elem => {

        // at least on last layer should be a new expression
        executed.contains(elem._2) match {
          case true =>
            // don't execute and dont' count

            // check if element is on last layer an already in filter
            elem._2.size match {
              case `depth` =>
                val filter = visited.apply(elem._2.size)
                filter.contains(elem._2) match {
                  case true => throw new Exception("is in filter should not be here")
                  case false => throw new Exception("why it is not filtered out?")
                }
              case _ => // everything fine if we are not on last layer
            }

          case false =>
            counter += 1
            hasExecuted = true
            executed.add(elem._2)
            val result = panel.f(elem._1)
          // result is handled by the executor
        }
      })

      if (hasExecuted == false) {
        throw new Exception("should always execute something")
      }

      // repeat
      solution = initialSolution
      path = Seq.empty[(Solution[P], Seq[Int])]

    }

    ExplorationResult(
      solution,
      best,
      None
    )
  }
}
