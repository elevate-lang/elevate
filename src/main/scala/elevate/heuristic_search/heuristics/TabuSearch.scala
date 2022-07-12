package elevate.heuristic_search.heuristics

import elevate.heuristic_search.util.{Path, Solution}
import elevate.heuristic_search.{Heuristic, HeuristicPanel}


class TabuSearch[P] extends Heuristic[P] {


  def start(panel: HeuristicPanel[P], initialSolution:Solution[P], depth: Int):(P, Option[Double], Path[P])  = {

    var solution = initialSolution
    var solutionValue:Option[Double] = panel.f(solution)
    val path = new Path(solution.expression, panel.f(solution))

    // initalize tabu list
    val tabuList = scala.collection.mutable.Queue.empty[Int]
    val tabuListSize = 10  // change this during exploration

    // main loop
    var k = 0
    while(k < 0) {
      println("layer: " + k)
      println("current: " + solution)


      // create neighborhood
      val Ns = panel.N(solution)
      print("Ns.size: " + Ns.size)

      Ns.foreach(ns => {

        // if ns is not in tabu list do this
        // else do nothing
        tabuList.contains(ns.hashCode()) match {
          case true => {
            val fns = panel.f(ns)
            val fsolution = solutionValue

            (fns, fsolution) match {
              case (Some(fnsInternal), Some(fsolutionInternal)) => {

                // check for new minimum
                if (fnsInternal < fsolutionInternal) {
                  solution = ns
                  solutionValue = fns

                  // update solution and add to path
                  solution = ns
                  path.add(ns.expression, ns.strategies.last, fns)

                  // update tabu list

                  // add element to list
                  tabuList.enqueue(solution.hashCode())

                  // remove first element if certain size is reached
                  if(tabuList.size > tabuListSize) {
                    tabuList.dequeue()
                  }
                }
              }
              case _ => // do nothing
            }
          }
          case false => // skip this element
        }
      })


      k = k + 1
    }


    null
  }

}
