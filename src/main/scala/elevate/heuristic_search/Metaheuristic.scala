package elevate.heuristic_search

import elevate.core.Strategy
import elevate.heuristic_search.heuristics.Random
import elevate.heuristic_search._
import elevate.heuristic_search.util.Path


// runner class
class Metaheuristic[P](val name:String,
                       val heuristic:Heuristic[P],
                       val depth:Int,
                       val iterations: Int,
                       val runner:Runner[P],
                       val strategies:Set[Strategy[P]],
                       val output:String
                      ) extends Runner[P] {

  def execute(solution: P): (P, Option[Double]) = {

    // new heuristicPanel with runner (is either new metaheuristic or executor)
    val panel = new HeuristicPanelImplementation[P](runner, strategies)

    // conduct heuristic using panel and configs like depth and iterations
    var best: (P, Option[Double]) = (solution, None)
    for (_ <- Range(0, iterations)) {
      val result = heuristic.start(panel, solution, depth)
      // print path
      result._3.writePathToDot(output + "/" + name + ".dot")
      result._3.writePathToDisk(output + "/" )

      best._2 match {
        case Some(currentBest) => {
          result._2 match {
            case Some(candidateBest) => {
              // check if new best is found
              if (candidateBest < currentBest) {
                best = (result._1, result._2)
              }
            }
            case _ => // do nothing
          }
        }
          // initialize best
        case None => {
          //just if there is some result
          result._2 match {
            case Some(_) => {
              // initialize best
              best = (result._1, result._2)
            }
            case _ => // do nothing
          }
        }
      }
    }

    best
  }
}

    // best in all iterations which represents the return value





    // make this general!
    // get this from configuration
//
//    // traverse runner path
//    name match {
//      case "C" => {
//        // lower
//        val lowered = elevate.rise.rules.lowering.lowerToC.apply(solution)
//        //execute
//        executeC(lowered, iterations)
//      }
//      case "OpenMP" => throw new Exception("not yet implemented")
//      case "OpenCL" => throw new Exception("not yet implemented")
//      case "Random" => {
//        println("start new search from: " + name)
//        var best:Option[Double] = None
//
//        // repeat random for iterations time
//        for(_ <- Range(0,iterations)){
//
//          // new MockupSearch instance
//          val version = new Exploration[P](runner, strategies)
//
//          // start random heuristic
//          val heuristic = new Random[P](solution, version, iterations)
//          heuristic.start()
//
//          // check for new best value (and/or initialize best variable)
//          (best, heuristic.best) match {
//            case (Some(a), Some(b)) => {
//              if (b < a) {
//                best = heuristic.best
//              }
//            }
//            case (None, Some(b)) =>{
//              best = Some(b)
//            }
//            case _ =>
//          }
//        }
//        best
//      }
//      case _ => {
//        throw new Exception("should never reach this point")
//      }
//    }

