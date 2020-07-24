package elevate.heuristic_search

import elevate.core.strategies.basic
import elevate.core.{Failure, Strategy, Success}


// encapsulates definition of neighbourhood
class HeuristicPanelImplementation[P](val runner:Runner[P], val strategies:Set[Strategy[P]]) extends HeuristicPanel[P] {

  val solutions = new scala.collection.mutable.HashMap[Int, Option[Double]]()

  def N(solution:P):Set[(P, Strategy[P])]= {
    val neighbours = scala.collection.mutable.Set[(P, Strategy[P])]()

    // try each strategy and add result to neighbourhood set
    strategies.foreach(strategy  => {
      try {
        // apply strategy
        val result = strategy.apply(solution)

        // check rewriting result and it add to neighbourhood set
        result match {
          case _:Success[P] => neighbours.add( (result.get,strategy) ) //add to neighbourhood
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
    neighbours.add((solution, identity))

    neighbours.toSet
  }

  // warning: check size of hashmap
  def f(solution:P): Option[Double] = {
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

