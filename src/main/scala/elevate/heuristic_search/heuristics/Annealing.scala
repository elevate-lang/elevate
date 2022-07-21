package elevate.heuristic_search.heuristics

import elevate.heuristic_search.util.{Path, Solution, hashProgram}
import elevate.heuristic_search.{ExplorationResult, Heuristic, HeuristicPanel}

import scala.collection.mutable.Stack

class Annealing[P] extends Heuristic[P] {


  // todo allow to go up


  def start(panel: HeuristicPanel[P], initialSolution: Solution[P], depth: Int, samples: Int): ExplorationResult[P] = {
    var solution = initialSolution

    val path = new Path(solution.expression, panel.f(solution), null, null, 0)

    // initialization of helper
    val random = scala.util.Random
    var T = 100.0
    val alpha = 0.9

    // main loop
    var k = 0
    var counter = 0


    // todo add parent object to neighborhood
    //    parent for backtracking?
    //      store parent in Seq

    // stack

    var parents = Stack[Solution[P]]()

    // use recursion (stack) for that?

    println("welcome to simulated annealing")

    while (counter < samples) {
      println("iteration: " + k)
      //      println("current: " + solution)
      k = k + 1
      // create neighborhood
      // filter out expressions to far rewritten
      println("rewrite")
      val NsChildren = panel.N(solution).filter(elem => elem.strategies.size <= depth)
      println("finished")

      // todo
      // should probably not pop here
      val Ns = parents.size match {
        case 0 => NsChildren
        case _ =>
          //          println("add parent to: " + NsChildren.size)
          NsChildren :+ parents.top
      }

      //      println("Ns: " + Ns.size)


      //      val Ns = NsChildren :+ parents.pop()
      // add parent to neighborhood

      // init solution and solutionCandidate runtimes and increase counter
      val solutionRuntime = panel.f(solution)
      var solutionCandidateRuntime = solutionRuntime

      var validCandidate = false

      var solutionCandidate: Solution[P] = null
      solution
      // get new solution candidate


      // todo
      // if we don't get a second chance
      // stay in this loop to explore all elements of Ns after the other
      // finally the parent one
      // if there is no expression left and no second chance -> terminte aldus
      while (!validCandidate) {
        //pick element from neighborhodd
        solutionCandidate = Ns.toSeq(random.nextInt(Ns.size))

        // rewrite
        // save state lambda
        val stateLambda = solution
        // execute
        counter += 1
        solutionCandidateRuntime = panel.f(solutionCandidate)
        // check if execution is valid
        solutionCandidateRuntime match {
          case Some(_) => {
            validCandidate = true
            //            println("candidate has valid runtime, so lets try it")
          }
          case None =>
          //            println("can't execute current expression")
        }
      }

      //      println("valid candidate")

      if (solutionRuntime.get > 0) {
        // second chance using probability function
        if (solutionCandidateRuntime.get > solutionRuntime.get) {
          print("worse - second chance? - ")
          // get random number
          val randomNumber = random.nextInt(100)
          // calculate probability

          //          println("runtime: " + solutionRuntime)
          //          println("runtime: " + solutionCandidateRuntime)
          //          println("T: " + T)
          //          println("randomNumber: " + randomNumber)
          //          println("f(s') - f(s): " + math.abs(solutionCandidateRuntime.get - solutionRuntime.get))

          val probability = (T * 100) / math.abs(solutionCandidateRuntime.get - solutionRuntime.get)
          //          println("probability: " + probability)

          // check number
          if (randomNumber > probability) {
            //            println("probability said no second chance for: " + solutionCandidateRuntime)
            println("no")
          } else {
            println("yes")
            //            println("probability said yes, second chance: " + solutionCandidateRuntime)


            val childIsParent = parents.size match {
              case 0 => false
              case _ => solutionCandidate.equals(parents.top)
            }

            //            println("parent or old solution : " + solution.strategies.mkString("[", ",", "]"))
            //            println("new solutoion iwll be: " + solutionCandidate.strategies.mkString("[", ",", "]"))
            //            println("childIsParent: " + childIsParent)

            if (childIsParent) {
              parents.pop()
              //              println("is parent")
              //              System.exit(0)
              //otherwise push the current parent to the stack
            } else {
              //              println("stack.size: " + parents.size)
              //              println("push to stack")
              parents.push(solution)
              //              println("stack.size: " + parents.size)
              //              parents.foreach(elem => println(elem.strategies.mkString("[", ",", "]")))
              //              val all = parents.popAll()
              //              println("all: " + all.size)
              //              all.foreach(elem => println(elem.strategies.mkString("[", ",", "]")))
              //              parents.pushAll(all)

              //              System.exit(0)
            }


            solution = solutionCandidate
          }
        } else {

          println("better")
          // if our new solution is the parent of the current, remove it from stack

          val childIsParent = parents.size match {
            case 0 => false
            case _ => solutionCandidate.equals(parents.top)

          }

          if (childIsParent) {
            parents.pop()
            //otherwise push the current parent to the stack
          } else {
            parents.push(solution)
          }
          solution = solutionCandidate
        }
      }
      // update Temperature T
      T = alpha * T
      println("T: " + T)

      //      println(s"stac:  ${parents.size}\n")
      //      parents.foreach(elem => println(elem.strategies.mkString("[", ",", "]")))

    }

    // best = null
    val best = null

    ExplorationResult(
      solution,
      best,
      Some(path)
    )
  }

}
