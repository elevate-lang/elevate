package elevate.heuristic_search.heuristics

import elevate.heuristic_search.util.{Path, SearchSpaceHelper, Solution}
import elevate.heuristic_search.{Heuristic, HeuristicPanel}

class RandomSampling[P] extends Heuristic[P] {
  // initialize global best
  var best: Option[Double] = None

  var sequence = scala.collection.mutable.Seq.empty[Int]

  var Ntotal = 0.0

  def start2(panel: HeuristicPanel[P], initialSolution: Solution[P], depth: Int): (P, Option[Double], Path[P]) = {

    var solution = initialSolution

    val path = new Path(solution.expression, panel.f(solution), null, null, 0)
    val random = scala.util.Random

    var output: (Solution[P], Option[Double]) = (initialSolution, panel.f(initialSolution))

    val max = 5


    var j = 0
    while (j < depth) {
      println("\n")
      println("iteration: " + j)
      j += 1

      val rewrites = random.nextInt(max)
      println("rewrites: " + rewrites)

      // start
      var i = 0
      sequence = sequence.empty
      while (i < rewrites) {

        println("i: " + i)

        // get neighborhood

        val NStart = System.currentTimeMillis()
        val ns = panel.N(solution)
        Ntotal += (System.currentTimeMillis() - NStart)

        println("ns.size: " + ns.size)

        if (ns.size == 0) {
          // backtrack

          // restore solution drop element

          // backtrack
          i -= 1

          // remove last number
          sequence = sequence.dropRight(1)

          // restore old solution
          solution = panel.getSolution(initialSolution, sequence.toSeq).get

        } else {

          val number = random.nextInt(ns.size)
          solution = ns.toSeq(number)
          sequence = sequence :+ number
          println(number.toString + " - sequ: " + sequence.mkString(","))

          i += 1
        }
      }
      println("sequence: " + sequence.mkString(","))

      // execute
      val performance = panel.f(solution)

      // update if better
      output = performance match {
        case Some(value) => {
          value < output._2.get match {
            case true => (solution, performance)
            case false => output
          }
        }
        case None => output
      }
    }
    (output._1.expression, output._2, path)
  }

  def start(panel: HeuristicPanel[P], initialSolution: Solution[P], depth: Int): (P, Option[Double], Path[P]) = {

    val totalStart = System.currentTimeMillis()

    var solution = initialSolution

    val path = new Path(solution.expression, panel.f(solution), null, null, 0)
    val random = scala.util.Random

    var output: (Solution[P], Option[Double]) = (initialSolution, panel.f(initialSolution))

    val max = 10

    var j = 0
    while (j < depth) {
      println("\n")
      println("iteration: " + j)
      j += 1

      val rewrites = random.nextInt(max)
      println("rewrites: " + rewrites)


      // todo avoid duplicates and implement real backtracking

      // start
      var i = 0
      sequence = sequence.empty
      while (i < rewrites) {
        println("i: " + i)
        var found = false

        // get neighborhood


        val NStart = System.currentTimeMillis()
        val ns = panel.N(solution)
        Ntotal += (System.currentTimeMillis() - NStart)
        //      println("ns: " + ns.size)

        // todo fix by replacing with real backtracking
        if (ns.size + 1 <= 1) {
          // backtrack
          i -= 1

          // remove last number
          sequence = sequence.dropRight(1)

          // restore old solution
          solution = panel.getSolution(initialSolution, sequence.toSeq).get

        } else {
          i += 1

          // check duplicates?
          while (!found) {

            // todo avoid duplicates
            val number = random.nextInt(SearchSpaceHelper.strategies.size)

            val numberCheck = panel.checkRewrite(solution, number)

            if (numberCheck) {
              solution = panel.getSolution(solution, Seq(number)).get
              found = true
              sequence = sequence :+ number
            }
          }
        }


      }
      println("sequence: " + sequence.mkString(","))

      // execute
      val performance = panel.f(solution)

      // update if better
      output = performance match {
        case Some(value) => {
          value < output._2.get match {
            case true => (solution, performance)
            case false => output
          }
        }
        case None => output
      }
    }


    val total = System.currentTimeMillis() - totalStart
    println("total: " + total)
    println("Ntotal: " + Ntotal)

    (output._1.expression, output._2, path)
  }
}
