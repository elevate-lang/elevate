package elevate.heuristic_search.heuristics

import elevate.heuristic_search.util.{Solution, hashProgram}
import elevate.heuristic_search.{ExplorationResult, Heuristic, HeuristicPanel}

import scala.collection.mutable
import scala.collection.mutable.Stack

class TabuSearch[P] extends Heuristic[P] {

  def start(
             panel: HeuristicPanel[P],
             initialSolution: Solution[P],
             depth: Int,
             samples: Int
           ): ExplorationResult[P] = {

    var solution: (Solution[P], Seq[Int]) = (initialSolution, Seq.empty[Int])

    var solutionValue: Option[Double] = panel.f(solution._1)

    // handle duplicates
    var executed = mutable.HashSet.empty[Seq[Int]]

    // init tabu list
    val tabuList = scala.collection.mutable.Queue.empty[Seq[Int]]
    val tabuListSize = 25 // change this during exploration

    var parents = Stack[(Solution[P], Seq[Int])]()

    // main loop
    var k = 0
    var counter = 0
    var ready = false

    // add starting epxression to paren
    //    parents.push((initialSolution, Seq.empty[Int]))

    // when is ready?
    while (counter < samples) {
      //      println(s"[${k}/${depth}]")
      //      println("layer: " + k)
      //      println("current: " + solution)


      // create neighborhood
      //      val Ns = panel.N(solution)
      val NsChildren: Seq[Solution[P]] = panel.N(solution._1).filter(elem => elem.strategies().size <= depth)

      val NsChildrenFull = NsChildren.zip(Range(0, NsChildren.size)).map(elem => {
        //        val nsRewrite: Seq[Int] = parents.size match {
        //          case 0 => Seq(elem._2)
        //          case _ => solution._2 :+ elem._2
        //        }
        (elem._1, solution._2 :+ elem._2)
      })

      val NsFiltered = NsChildrenFull.filter(elem => !tabuList.contains(elem._2))


      // add parent after filtering -> always allow to go up
      val Ns: Seq[(Solution[P], Seq[Int])] = parents.size match {
        case 0 => NsFiltered
        case _ =>
          //          println("add parent to: " + NsChildren.size)
          NsFiltered :+ parents.top
      }

      println(s"\n[${k}/${samples}] : ")

      println("current: " + solution._2.mkString("[", ", ", "]"))
      val parent = parents.size match {
        case 0 => Seq.empty[Int]
        case _ => parents.top._2
      }
      println("parent was: " + parent.mkString("[", ", ", "]"))
      println("NsChildrenFull: " + NsChildrenFull.size)
      println("NsFiltered: " + NsFiltered.size)
      println("Ns (with parent): " + Ns.size)
      println("Ns before filtering")
      NsChildrenFull.foreach(elem => {
        println(elem._2.mkString("[", ", ", "]"))
      })

      println(s"filter: ${tabuList.size}")
      tabuList.foreach(elem => println(elem.mkString("[", ", ", "]")))
      println("\n")

      println("Ns filtered with parent: ")
      Ns.foreach(elem => println(elem._2.mkString("[", ", ", "]")))
      println("\n")

      // (solution[P], Seq[Int], Option[Double])
      // get minimum

      // if nothing left to explore terminate
      if (Ns.isEmpty) {
        return ExplorationResult(
          solution = solution._1,
          performance = None,
          searchSpace = None
        )
      }

      val NsResult = Ns.map(ns => {

        println("try: " + ns._2.mkString("[", ", ", "]"))


        // execute, collect and take minimum in the end

        // if ns is not in tabu list do this
        // else do nothing

        //        val nsRewrite: Seq[Int] = parents.size match {
        //          case 0 => Seq(nIndex)
        //          case _ => parents.top._2 :+ nIndex
        //        }
        //        nIndex += 1

        //        tabuList.contains(ns) match {
        //          case false => {
        println(s"[${counter}/${samples}] tabu search")


        // handle duplicates
        // we execute (should be buffered anyways)
        // we could use hashmap instead of hashset here, e.g. HashMap[Seq[Int], Option[Double]]
        // buffer the runtimes ourselves
        val fns = executed.contains(ns._2) match {
          case true => // execute but don't count

            panel.f(ns._1)
          case false => // execute, count and add
            counter += 1
            executed.add(ns._2)

            panel.f(ns._1)
        }

        (ns._1, ns._2, fns)
      })

      // get min of neighborhood
      val NsMin = NsResult.reduceLeft((a, b) => {
        a._3 match {
          case Some(a_value) => b._3 match {
            case Some(b_value) => a_value < b_value match {
              case true => a
              case false => b
            }
            case None => a
          }
          case None => b._3 match {
            case Some(_) => b
            case None => a
          }
        }
      })

      println("min is: " + NsMin._2.mkString("[", ", ", "]"))

      val parentCandidate = solution

      // update solution
      solution = (NsMin._1, NsMin._2)

      // update parents (aspiration)
      val childIsParent = parents.size match {
        case 0 => false
        case _ => solution.equals(parents.top)
      }

      if (childIsParent) {
        parents.pop()
      } else {
        parents.push(parentCandidate)
      }

      // update tabu list
      if (solution._2.size == depth) {
        tabuList.enqueue(solution._2)
      }

      // never dequeue
      //      if (tabuList.size > tabuListSize) {
      //        tabuList.dequeue()
      //      }

      //        fns
      //
      //        //            counter += 1
      //
      //        //            val fns = panel.f(ns)
      //        val fsolution = solutionValue
      //
      //
      //        (fns, fsolution) match {
      //          case (Some(fnsInternal), Some(fsolutionInternal)) => {
      //
      //            // check for new minimum
      //            if (fnsInternal < fsolutionInternal) {
      //              //                  solution = ns
      //              solutionValue = fns
      //
      //              // update solution and add to path
      //              //                  solution = ns
      //              //                  path.add(ns, fns)
      //
      //              // check parents
      //
      //              val childIsParent = parents.size match {
      //                case 0 => false
      //                case _ => solution.equals(parents.top)
      //              }
      //
      //              if (childIsParent) {
      //                parents.pop()
      //              } else {
      //                parents.push(solution)
      //              }
      //              //
      //
      //              //                  solution = (ns, nsRewrite)
      //              solution = ns
      //
      //              // update tabu list
      //
      //              // add element to list
      //              // todo adjust this
      //              tabuList.enqueue(ns._2)
      //
      //              // remove first element if certain size is reached

      //            } else {
      //
      //              val childIsParent = parents.size match {
      //                case 0 => false
      //                case _ => solution.equals(parents.top)
      //              }
      //
      //              if (childIsParent) {
      //                parents.pop()
      //              } else {
      //                parents.push(solution)
      //              }
      //              //
      //
      //              //                  solution = (ns, nsRewrite)
      //              solution = ns
      //
      //
      //              // add as well
      //              tabuList.enqueue(ns._2)

      //            }
      //          }
      //          case _ => // do nothing, not better
      //        }
      //          }
      //          case true => // skip this element it's tabu!!
      //        }

      //        if (counter == samples) {
      //          ready = true
      //          //          ready
      //          //           todo adjust this
      //          //          ExplorationResult(
      //          //            solution = solution,
      //          //            performance = None,
      //          //            searchSpace = None
      //          //          )
      //        }


      // do we need a k here?
      k = k + 1
    }

    // todo adjust this
    ExplorationResult(
      solution = solution._1,
      performance = None,
      searchSpace = None
    )
  }
}
