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


  // todo optimize this
  def computeSample2(panel: HeuristicPanel[P], initialSolution: Solution[P], numbers: Seq[Int]): Solution[P] = {


    var solution = initialSolution
    strategies.foreach(strategy => {

      // get neighbourhood
      val Ns = panel.N(solution)
      Ns.foreach(ns => {

        if (ns.strategies.last.toString().equals(strategy)) {
          // apply!
          solution = ns
        }
        // check if this is your number
      })
    })

    solution
  }


  def getSolution(initial: Solution[P], numbers: Seq[Int]): Option[Solution[P]] = {

//    println("getSolution for: " + numbers.mkString("[", ", ", "]"))

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
      case e: Throwable => None
    }

    //    println("solution: " + hashSolution(tmp))
    //    println("\n")
  }


  // parallel without checking
  def N3(solution: Solution[P]): Set[Solution[P]]= {
    call += 1

//    val Ns = strategies.map(strategy => {
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

  def N2(solution: Solution[P]): Set[Solution[P]]= {

    call += 1
//    val neighbours = scala.collection.mutable.Set[Solution[P]]()

    // try each strategy and add result to neighbourhood set

    val NsOptions = strategies.map(strategy => {
//      val NsOptions  = strategies.par.map(strategy => {
      try {
        //        println("apply strategy")
        // apply strategy
        val result = strategy.apply(solution.expression)
        //        println("finished")

        // check rewriting result and it add to neighbourhood set
        result match {
          case _:Success[P] => {
            // check if expression is valid
            val newSolution = new Solution[P](result.get, solution.strategies :+ strategy)

            if(runner.checkSolution(newSolution)){

//              neighbours.add(newSolution)
              //                new Solution[P](result.get, solution.strategies :+ strategy))
              //add to neighbourhood
              Some(newSolution)
            }else{
              // do nothing, drop result/ candidate

              None
            }
          }
          case _:Failure[P] => None
        }
      }catch{
        case e:Throwable => None
      }
    })
//    val Ns = NsOptions.seq.flatten
      val Ns = NsOptions.flatten
    //    val identity = basic.id[P]

    // add id to neighbourhood (use real id strategy instead of null)
    //    neighbours.add(new Solution[P](solution.expression, solution.strategies :+ identity))

//    neighbours.toSet
    Ns
  }

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

