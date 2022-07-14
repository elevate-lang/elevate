package elevate.heuristic_search.heuristics

import elevate.heuristic_search.util.{Path, Solution, hashProgram}
import elevate.heuristic_search.{ExplorationResult, Heuristic, HeuristicPanel}


class TabuSearch[P] extends Heuristic[P] {


  def start(
             panel: HeuristicPanel[P],
             initialSolution: Solution[P],
             depth: Int
           ): ExplorationResult[P] = {

    var solution = initialSolution
    var solutionValue: Option[Double] = panel.f(solution)
    val path = new Path(
      program = solution.expression,
      value = panel.f(solution),
      initial = null,
      current = null,
      elements = 0
    )

    // initalize tabu list
    val tabuList = scala.collection.mutable.Queue.empty[String]
    val tabuListSize = 10 // change this during exploration

    // main loop
    var k = 0
    while (k < 0) {
      println("layer: " + k)
      println("current: " + solution)


      // create neighborhood
      val Ns = panel.N(solution)
      print("Ns.size: " + Ns.size)

      Ns.foreach(ns => {

        // if ns is not in tabu list do this
        // else do nothing
        tabuList.contains(hashProgram(ns.expression)) match {
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
                  path.add(ns, fns)

                  // update tabu list

                  // add element to list
                  tabuList.enqueue(hashProgram(solution.expression))

                  // remove first element if certain size is reached
                  if (tabuList.size > tabuListSize) {
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

    // todo adjust this
    ExplorationResult(
      solution = solution,
      performance = None,
      searchSpace = None
    )
  }

}
