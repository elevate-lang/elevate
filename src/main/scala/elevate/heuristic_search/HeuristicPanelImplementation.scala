package elevate.heuristic_search

import elevate.core.strategies.basic
import elevate.core.{Failure, Strategy, Success}
import elevate.heuristic_search.util.SearchSpaceHelper.strategies
import elevate.heuristic_search.util.{SearchSpaceHelper, Solution, hashProgram, hashSolution}

import scala.collection.mutable.ListBuffer
import scala.collection.parallel.CollectionConverters._
import scala.collection.parallel.ParSet


// encapsulates definition of neighbourhood
class HeuristicPanelImplementation[P](val runner:Runner[P], val strategies:Set[Strategy[P]]) extends HeuristicPanel[P] {

  val solutions = new scala.collection.mutable.HashMap[String, Option[Double]]()
  var call = 0

  def getSolution(initial: Solution[P], numbers: Seq[Int]): Option[Solution[P]] = {

    println("getSolution for: " + numbers.mkString("[", ", ", "]"))

    // get strategies as string
    val strategiesString = SearchSpaceHelper.getStrategies(numbers)

    // get strategies from strings
    var strategiesMap = Map.empty[String, Strategy[P]]
    strategies.foreach(strat => strategiesMap += (strat.toString() -> strat))

    // rewrite expression
    var solution = initial
    try {

      strategiesString.foreach(strat => {
        //        println("look for: " + strat)
        val strategy = strat match {
          case "id" => basic.id[P]
          case _ => strategiesMap.apply(strat)
        }
        solution = new Solution[P](strategy.apply(solution.expression).get, solution.strategies :+ strategy)
      })
      Some(solution)
    } catch {
      case e: Throwable => {
        throw new Exception("Could not reproduce rewrites: " + e)
        None
      }
    }

    //    println("solution: " + hashSolution(tmp))
    //    println("\n")
  }

  // parallel without checking
  def N3(solution: Solution[P]): Set[Solution[P]]= {
    call += 1

      val Ns = strategies.par.map(strategy => {
      try {
        val result = strategy.apply(solution.expression)
        result match {
          case _:Success[P] => Some(new Solution[P](result.get, solution.strategies :+ strategy))
          case _:Failure[P] => None
        }
      }catch{
        case e:Throwable => None
      }
    })

//    Ns.flatten
    Ns.seq.flatten
  }

  def N(solution: Solution[P]): Set[Solution[P]]= {

    call += 1

    val NsOptions = strategies.map(strategy => {
//      val NsOptions  = strategies.par.map(strategy => {
      try {

        val result = strategy.apply(solution.expression)

        result match {
          case _:Success[P] => Some(new Solution[P](result.get, solution.strategies :+ strategy)).filter(runner.checkSolution)
          case _:Failure[P] => None
        }
      } catch {
        case e:Throwable => None
      }
    })
//    val Ns = NsOptions.seq.flatten
      val Ns = NsOptions.flatten

    // add id to neighbourhood (use real id strategy instead of null)
    //    val identity = basic.id[P]
    //    neighbours.add(new Solution[P](solution.expression, solution.strategies :+ identity))

    Ns
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

