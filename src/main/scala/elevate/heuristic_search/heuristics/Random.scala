package elevate.heuristic_search.heuristics

import elevate.core.Strategy
import elevate.heuristic_search.util.{Path, Solution, hashProgram, hashSolution}
import elevate.heuristic_search._

import scala.Console.flush

class Random[P] extends Heuristic[P] {
  // initialize global best
  var best: Option[Double] = None

  def start(panel: HeuristicPanel[P], initialSolution: Solution[P], depth: Int, samples: Int): ExplorationResult[P] = {
    var solution = initialSolution
    //    var solution = new Solution[P](initialSolution, scala.collection.mutable.Seq.empty[Strategy[P]])

    val path = new Path(solution.expression, panel.f(solution), null, null, 0)
    val random = scala.util.Random

    // iterations in total
    val maxDepth = depth

    // how many rounds
    val rounds = samples / depth

    for (i <- Range(0, rounds)) {
      //      println(s"Iteration: [${i}]")
      flush()

      // repeat
      println("solution: " + hashProgram(solution.expression))
      println("strategos: " + solution.strategies.size)
      solution.strategies.foreach(println)

      println("get random number ")
      //      val depth = random.nextInt(maxDepth - 1) // make sure we don't have zero
      val depth = maxDepth
      println(s"[${i}] : depth: ${depth}")

      for (k <- Range(0, depth + 1)) {
        println(s"Rewrite: [${k}]")

        val Ns = panel.N(solution)
        //        println("rewrite finished")
        flush()
        val size = Ns.size
        println("Neighbourhood size: " + size)
        var valid = false
        var j = 0

        //        Ns.foreach(elem => {
        //          println("solution: " + hashSolution(elem) + "  --  " + elem.strategies.mkString("[", ", ", "]"))
        //        })

        while (!valid && j < size) {
          //          println("loop started")
          flush()
          //          println(s"j: ${j}")
          j = j + 1
          //          println("throw dice")
          val randomIndex = random.nextInt(size)
          //          println("throw dice finished")

          //          println("get result from Ns")
          val result = Ns.toSeq(randomIndex)

          //          println("got result from Ns")

          //        val oldSolution = solution
          //        val oldSolutionValue = solutionValue
          //        solution = result

          //          val current = path.current

          //          println("execute ... ")
          flush()
          panel.f(result) match {
            case Some(value) =>
              valid = true
              //              val oldSolution = solution
              //              println(value)
              solution = result
              //add to path
              //              println("path.add")
              //              path.add(solution, Some(value))
              //              println("path.add finished")

              //              println("check best")
              // check if new global best is found
              best match {
                case None => best = Some(value)
                case Some(_) =>
                  value < best.get match {
                    case true => best = Some(value)
                    case false =>
                  }
              }
            //              println("check best finished")
            case None =>
            //              println("None")
            //              path.add(result, None) // why do we add this here?
            //              path.add(Solution(current.solution.expression, current.solution.strategies ++ Seq(elevate.core.strategies.basic.revert)), current.value) // and why this?
          }
          //          println("loop finished")
          flush()
        }
      }

      //      print("backtrack ...")
      // backtrack to initial
      //      var up = path.current
      //      while (up.predecessor != null) {
      //        up = up.predecessor
      //        path.add(Solution(up.solution.expression, up.solution.strategies ++ Seq(elevate.core.strategies.basic.revert)), up.value)
      //      }

      // reset solution
      // todo maybe keep track of global solution
      solution = initialSolution
      println("finished \n")
    }

    ExplorationResult(
      solution,
      best,
      Some(path)
    )
  }

}
