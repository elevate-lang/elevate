package elevate.heuristic_search

import elevate.core.strategies.basic
import elevate.core.strategies.traversal.oncetd
import elevate.core.{Failure, Strategy, Success}
import elevate.heuristic_search.util.Solution
import elevate.rise.Rise
import elevate.rise.strategies.normalForm.LCNF


// encapsulates definition of neighbourhood
class HeuristicPanelImplementation[P](val runner:Runner[P], val strategies:Set[Strategy[P]]) extends HeuristicPanel[P] {

  val solutions = new scala.collection.mutable.HashMap[Int, Option[Double]]()
  var call = 0

  def N(solution:Solution[P]):Set[Solution[P]]= {
    println("\n call number: " + call + "---------------------------------------------------")
    println("solution.strategy: " + solution.strategies.size)
    solution.strategies.foreach(elem =>{
      println("strategy: " + elem)
    })
    println
    call += 1
    val neighbours = scala.collection.mutable.Set[Solution[P]]()

    // try each strategy and add result to neighbourhood set
    var row = 0
    strategies.foreach(strategy  => {
//      println("row: " + row)
      row += 1
      try {
        // apply strategy
        val result = strategy.apply(solution.expression)

        // check rewriting result and it add to neighbourhood set
        result match {
          case _:Success[P] => {
            println("solution.strategies: \n" + solution.strategies)
            println("strategy: " + strategy)
            val tmp = solution.strategies :+ strategy
            println("update: \n" + tmp)
            neighbours.add(new Solution[P](result.get, solution.strategies :+ strategy))
            //add to neighbourhood
          }
          case _:Failure[P] => //nothing
        }
      }catch{
        case e:Throwable => {
          print("rewriting error: " + e +  "\n")
        }
      }
    })

    val identity = basic.id[P]

    // add id to neighbourhood (use real id strategy instead of null)
    neighbours.add(new Solution[P](solution.expression, solution.strategies :+ identity))

    neighbours.toSet
  }

  // warning: check size of hashmap
  def f(solution:Solution[P]): Option[Double] = {
    // buffer performance values in hashmap
    solutions.get(solution.hashCode()) match {
      case Some(value) => solutions.get(solution.hashCode()).get
      case _ => {
        val performanceValue = runner.execute(solution)._2
        solutions.+=(solution.hashCode() -> performanceValue)
        performanceValue
      }
    }
  }
}

