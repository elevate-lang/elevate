package elevate.heuristic_search.heuristics

import elevate.heuristic_search.util.{Path, Solution, hashProgram}
import elevate.heuristic_search.{ExplorationResult, Heuristic, HeuristicPanel}

import scala.collection.mutable.Stack

class Annealing[P] extends Heuristic[P] {


  val temperatureRandom = scala.util.Random


  def start(panel: HeuristicPanel[P], initialSolution: Solution[P], depth: Int, samples: Int): ExplorationResult[P] = {
    var solution = initialSolution

    val path = new Path(solution.expression, panel.f(solution), null, null, 0)

    // initialization of helper
    val random = scala.util.Random
    var T = 1000.0
    val alpha = 0.99
    val betterThreshold = 0.9

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
      println(s"[${counter + 1}/${samples}] annealing")
      //      println("current: " + solution)
      k += 1
      // create neighborhood
      // filter out expressions to far rewritten
      //      println("rewrite")
      val NsChildren = panel.N(solution).filter(elem => elem.strategies.size <= depth)
      //      println("finished")

      // todo
      // should probably not pop here
      var Ns = parents.size match {
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
        //        println("Ns before: " + Ns.size)
        Ns = Ns.filter(elem => {

          // don't remove parent (always allow to go back)
          parents.size match {
            case 0 =>
              //              println("no parent")
              !elem.equals(solutionCandidate)
            case _ =>
              elem.equals(parents.top) match {
                case true =>
                  //                  println("don't filter out parent")
                  true
                case false =>
                  !elem.equals(solutionCandidate)
              }
          }
        }
        )
        //        println("Ns after: " + Ns.size)

        // rewrite
        // save state lambda
        val stateLambda = solution
        // execute
        counter += 1
        solutionCandidateRuntime = panel.f(solutionCandidate)
        // check if execution is valid
        //        solutionCandidateRuntime match {
        //          case Some(_) => {
        //            validCandidate = true
        //            println("candidate has valid runtime, so lets try it")
        //      }
        //          case None =>
        //            println("can't execute current expression")
        //        }

        //      println("valid candidate")

        validCandidate = Ns.size match {
          case 0 => throw new Exception("there should always be a parent")
          case 1 => // only parent left & budget left -> wild-card
            true
          case _ =>
            // normal case
            solutionRuntime match {
              case Some(solutionValue) => solutionCandidateRuntime match {
                case Some(solutionCandidateValue) =>
                  (solutionCandidateValue / solutionValue) < betterThreshold match {
                    case true => true
                    case false => secondChance(solutionValue, solutionCandidateValue, T)
                  }
                case None =>
                  secondChance(solutionValue, 0, T)
              }
              case None => solutionCandidateRuntime match {
                case Some(_) => true
                case None =>
                  secondChance(0, 1, T) // prefer new solution a little
              }
            }
        }


        validCandidate match {
          case true => {
            //            println("new solution")
            // if our new solution is the parent of the current, remove it from stack

            if (parents.size match {
              case 0 => false
              case _ => solutionCandidate.equals(parents.top)
            }) {
              parents.pop()
              //otherwise push the current parent to the stack
            } else {
              parents.push(solution)
            }
            solution = solutionCandidate

          }
          case false => // ignore?
          //            println("no new solution")
          // do anything?
        }
      }

      //
      //      solutionRuntime match {
      //        case Some(value) =>
      //          //        if (solutionRuntime.get > 0) {
      //          // second chance using probability function
      //          if (solutionCandidateRuntime.get > solutionRuntime.get) {
      //            print("worse - second chance? - ")
      //            // get random number
      //            val randomNumber = random.nextInt(100)
      //            // calculate probability
      //
      //            //          println("runtime: " + solutionRuntime)
      //            //          println("runtime: " + solutionCandidateRuntime)
      //            //          println("T: " + T)
      //            //          println("randomNumber: " + randomNumber)
      //            //          println("f(s') - f(s): " + math.abs(solutionCandidateRuntime.get - solutionRuntime.get))
      //
      //            val probability = (T * 100) / math.abs(solutionCandidateRuntime.get - solutionRuntime.get)
      //            //          println("probability: " + probability)
      //
      //            // check number
      //            if (randomNumber > probability) {
      //              //            println("probability said no second chance for: " + solutionCandidateRuntime)
      //              println("no")
      //            } else {
      //              println("yes")
      //              //            println("probability said yes, second chance: " + solutionCandidateRuntime)
      //
      //
      //              val childIsParent = parents.size match {
      //                case 0 => false
      //                case _ => solutionCandidate.equals(parents.top)
      //              }
      //
      //              //            println("parent or old solution : " + solution.strategies.mkString("[", ",", "]"))
      //              //            println("new solutoion iwll be: " + solutionCandidate.strategies.mkString("[", ",", "]"))
      //              //            println("childIsParent: " + childIsParent)
      //
      //              if (childIsParent) {
      //                parents.pop()
      //                //              println("is parent")
      //                //              System.exit(0)
      //                //otherwise push the current parent to the stack
      //              } else {
      //                //              println("stack.size: " + parents.size)
      //                //              println("push to stack")
      //                parents.push(solution)
      //                //              println("stack.size: " + parents.size)
      //                //              parents.foreach(elem => println(elem.strategies.mkString("[", ",", "]")))
      //                //              val all = parents.popAll()
      //                //              println("all: " + all.size)
      //                //              all.foreach(elem => println(elem.strategies.mkString("[", ",", "]")))
      //                //              parents.pushAll(all)
      //
      //                //              System.exit(0)
      //              }
      //
      //
      //              solution = solutionCandidate
      //            }
      //          } else {
      //
      //            println("better")
      //            // if our new solution is the parent of the current, remove it from stack
      //
      //            val childIsParent = parents.size match {
      //              case 0 => false
      //              case _ => solutionCandidate.equals(parents.top)
      //
      //            }
      //
      //            if (childIsParent) {
      //              parents.pop()
      //              //otherwise push the current parent to the stack
      //            } else {
      //              parents.push(solution)
      //            }
      //            solution = solutionCandidate
      //          }
      //        case None =>
      //      }

      // update Temperature T
      T = alpha * T
      println(s"[${counter + 1}/${samples}] T: ${T}")

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

  def secondChance(
                    solutionRuntime: Double,
                    solutionCandidateRuntime: Double,
                    temperature: Double
                  ): Boolean = {

    // calculate probability

    // check if we have a value


    //          println("runtime: " + solutionRuntime)
    //          println("runtime: " + solutionCandidateRuntime)
    //          println("T: " + T)
    //          println("randomNumber: " + randomNumber)
    //          println("f(s') - f(s): " + math.abs(solutionCandidateRuntime.get - solutionRuntime.get))

    // get random number
    val randomNumber = temperatureRandom.nextInt(100)
    val probability = (temperature * 100) / math.abs(solutionCandidateRuntime - solutionRuntime)

    // decide
    randomNumber > probability
  }

}
