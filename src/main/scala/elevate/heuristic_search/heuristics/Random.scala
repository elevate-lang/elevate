package elevate.heuristic_search.heuristics

import elevate.core.Strategy
import elevate.heuristic_search.util.{Path, Solution}
import elevate.heuristic_search.{Heuristic, HeuristicPanel}

class Random[P] extends Heuristic[P]:
  // initialize global best
  var best:Option[Double] = None

  def start(panel:HeuristicPanel[P], initialSolution:Solution[P], depth: Int): (P, Option[Double], Path[P]) =
    var solution = initialSolution

//    var solution = new Solution[P](initialSolution, scala.collection.mutable.Seq.empty[Strategy[P]])

    val path = new Path(solution.expression, panel.f(solution))
    val random = scala.util.Random

    for (_ <- Range(0, depth))
      val Ns = panel.N(solution)
      var valid = false
      var j = 0
      while
        !valid && j < Ns.size
      do  
        j = j + 1
        val randomIndex = random.nextInt(Ns.size)
        val result = Ns.toSeq(randomIndex)
        panel.f(result) match
          case Some(value) =>
            valid = true
            solution = result
            //add to path
            path.add(solution.expression, result.strategies.last, Some(value))

            // check if new global best is found
            best match
              case None => best = Some(value)
              case Some(_) =>
                value < best.get match
                  case true => best = Some(value)
                  case false =>
          case _ =>

    (solution.expression, best, path)
  end start

end Random
