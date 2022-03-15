package elevate.heuristic_search

import elevate.core.strategies.basic
import elevate.core.{Failure, Strategy, Success}
import elevate.heuristic_search.util.{Solution, hashProgram}


// encapsulates definition of neighbourhood
class HeuristicPanelImplementation[P](val runner:Runner[P], val strategies:Set[Strategy[P]]) extends HeuristicPanel[P] {

  val solutions = new scala.collection.mutable.HashMap[String, Option[Double]]()
  var call = 0

  def N(solution: Solution[P]): Set[Solution[P]]= {
//    println("\n call number: " + call + "---------------------------------------------------")
//    println("solution.strategy: " + solution.strategies.size)
//    solution.strategies.foreach(elem =>{
//      println("strategy: " + elem)
//    })
//    println()
    call += 1
    val neighbours = scala.collection.mutable.Set[Solution[P]]()

    // try each strategy and add result to neighbourhood set
    var row = 0
    strategies.foreach(strategy  => {
//      println("row: " + row)
      row += 1
      try {
//        println("apply strategy")
        // apply strategy
        val result = strategy.apply(solution.expression)
//        println("finished")

        // check rewriting result and it add to neighbourhood set
        result match {
          case _:Success[P] => {
//            println("solution.strategies: \n" + solution.strategies)
//            println("strategy: " + strategy)
//            val tmp = solution.strategies :+ strategy

            // check if expression is valid
            val newSolution = new Solution[P](result.get, solution.strategies :+ strategy)
            if(runner.checkSolution(newSolution)){

              neighbours.add(newSolution)
//                new Solution[P](result.get, solution.strategies :+ strategy))
              //add to neighbourhood
            }else{
              // do nothing, drop result/ candidate

            }
          }
          case _:Failure[P] => //nothing
        }
      }catch{
        case e:Throwable => {
//          print("rewriting error: " + e +  "\n")
        }
      }
    })

//    val identity = basic.id[P]

    // add id to neighbourhood (use real id strategy instead of null)
//    neighbours.add(new Solution[P](solution.expression, solution.strategies :+ identity))

    neighbours.toSet
  }

  // warning: check size of hashmap
  def f(solution:Solution[P]): Option[Double] = {
    // buffer performance values in hashmap
    solutions.get(hashProgram(solution.expression)) match {
      case Some(value) => solutions.get(hashProgram(solution.expression)).get
      case _ => {
        val performanceValue = runner.execute(solution)._2
        solutions.+=(hashProgram(solution.expression)-> performanceValue)
        performanceValue
      }
    }
  }
}

