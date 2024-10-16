package elevate.heuristic_search.heuristics

import elevate.core.Strategy
import elevate.heuristic_search._
import elevate.heuristic_search.util.{RewriteIdentifier, Solution}

import scala.collection.mutable

class TabuSearchPlain[P] extends Heuristic[P] {
  // initialize global best
  var best: Option[Double] = None


  def start(panel: HeuristicPanel[P], initialSolution: Solution[P], depth: Int, samples: Int): ExplorationResult[P] = {

    var solution = initialSolution

    val random = scala.util.Random

    // avoid duplicates ( we don't need this for now)
    //    var visited = mutable.HashMap.empty[Int, mutable.HashSet[Seq[Int]]]

    //    // add empty foreach layer
    //    Range(0, depth + 1).foreach(layer => visited.addOne(layer, mutable.HashSet.empty[Seq[Int]]))

    var tabuList = scala.collection.mutable.Queue.empty[Seq[RewriteIdentifier[P]]] // can we collapse a sequence of rewrite identifiers?
    val tabuListSize = 100

    var counter = 0

    val throwAway = panel.f(solution)

    val its = 10

    // count executed or all solutions?
    while (counter < its) {
      println("\n")
      println(s"[${counter}/${its}]: ")
      println(s"Solution: ${solution.rewrites().mkString("[", ", ", "]")}")
      println(s"tabuList: ${tabuList.size}")
      tabuList.foreach(elem => println(elem.mkString("[", ", ", "]")))

      // local search (iterative improvement) with tabu list

      // get neighbourhood
      val Ns = panel.N(solution)

      // filter using tabu list
      val NsFiltered = Ns
        .filter(elem => !tabuList.contains(elem.rewrites())) // filter out according to tabu list
        .filter(elem => !elem.rewrites().equals(solution.rewrites())) // filter out current solution (avoid getting stuck at this)

      // eval neighbourhood
      val NsF = NsFiltered.map(ne => (ne, panel.f(ne)))

      println("Neighborhood: ")
      NsF.foreach(elem => {
        println(s"${elem._2}:  ${elem._1.rewrites().mkString("[", ", ", "]")}")
      })



      // add aspiration criteria
      // todo

      // catch case of emtpy neighborhood
      // maybe throw exception
      if (NsF.isEmpty) {
        return ExplorationResult(
          solution,
          best,
          None
        )
      }

      // get best from neighbourhood
      // reduce minimum
      val min = NsF.reduceLeft((sMin, sCand) => {
        sMin._2 match {
          case Some(sMinValue) => sCand._2 match {
            case Some(sCandValue) => sCandValue < sMinValue match {
              case true => sCand
              case false => sMin
            }
            case None => sMin
          }
          case None => sCand._2 match {
            case Some(_) => sCand
            case None => sMin
          }
        }
      })

      // add parent to tabu list
      // allow to go back
      tabuList = tabuList.addOne(solution.rewrites())

      // update solution
      solution = min._1

      // update tabu list
      // add best 5 elements to tabu list
      val NsFSorted = NsF.filter(elem => !elem._2.isEmpty).map(elem => (elem._1, elem._2.get)).sortWith(_._2 < _._2)

      // add first quarter of elements to neighborhood
      //      beset
      NsFSorted.take(NsFiltered.size / 4).foreach(elem => tabuList = tabuList.addOne(elem._1.rewrites()))

      // worset?
      //      NsFSorted.takeRight(NsFiltered.size / 2).foreach(elem => tabuList = tabuList.addOne(elem._1.rewrites()))

      // add all elements to tab lust
      //      NsF.foreach(elem => tabuList = tabuList.addOne(elem._1.rewrites()))

      // check if tabu list is to big

      // don't remove elements from tabu list
      //      val difference = tabuList.size - tabuListSize
      //      if (difference > 0) {
      //        Range(0, difference).foreach(_ => tabuList.dequeue())
      //      }

      // check size of neighborhood

      // repeat

      counter += 1

    }


    ExplorationResult(
      solution,
      best,
      None
    )
  }
}
