package elevate.heuristic_search.panel_implementations

import elevate.core.strategies.basic
import elevate.core.{Failure, Strategy, Success}
import elevate.heuristic_search.util.{SearchSpaceHelper, Solution, SolutionStep, hashProgram, hashSolution}
import elevate.heuristic_search.{HeuristicPanel, Runner}

import scala.collection.parallel.CollectionConverters._

// encapsulates definition of neighbourhood
class StandardPanel[P](
                        val runner: Runner[P],
                        val strategies: Seq[Strategy[P]],
                        val afterRewrite: Option[Strategy[P]] = None, // e.g. rewrite normal form
                        val beforeExecution: Option[Strategy[P]] = None, // e.g. code-gen normal form
                        val rewriteFunction: Option[Solution[P] => Seq[Solution[P]]] = None,
                        val importExport: Option[(String => Solution[P], (Solution[P], String) => Unit)],
                      ) extends HeuristicPanel[P] {

  val solutions = new scala.collection.mutable.HashMap[String, Option[Double]]()
  var call = 0

  // do we want to have a default mode
  def N(solution: Solution[P]): Seq[Solution[P]] = {
    rewriteFunction match {
      // expand strategy mode
      case Some(rewriteFunction) =>
        val result: Seq[Solution[P]] = afterRewrite match {
          case Some(aftermath) =>
            val candidates: Seq[Solution[P]] = rewriteFunction.apply(solution)

            // todo check aftermath
            // rewriteFunction.apply(solution).map(elem => Solution(aftermath.apply(elem.expression).get, elem.strategies))

            candidates
          case None =>
            rewriteFunction.apply(solution)
        }
        result
      // default mode
      case None => N_default(solution)
    }
  }

  // todo check this implementation
  def N_default(solution: Solution[P]): Seq[Solution[P]] = {

    call += 1

    val NsOptions: Seq[Option[Solution[P]]] = strategies.map(strategy => {
      //      val NsOptions  = strategies.par.map(strategy => {
      try {

        val result = strategy.apply(solution.expression())

        val output: Option[Solution[P]] = result match {
          case _: Success[P] =>

            //            Some(new Solution[P](result.get, solution.strategies() :+ strategy)).filter(runner.checkSolution)
            val step: SolutionStep[P] = SolutionStep[P](
              expression = result.get,
              strategy = strategy,
              location = 0
            )

            //            Solution[P](solution.solutionSteps :+ SolutionStep[P](result.get, strategy, 0))

            Some(Solution[P](
              solutionSteps = solution.solutionSteps :+ step
            ))

          case _: Failure[P] =>
            //              println("failure: " + result.toString)
            None
        }

        output
      } catch {
        case e: Throwable => None
      }
    })
    //    val Ns = NsOptions.seq.flatten
    val Ns = NsOptions.flatten

    Ns
  }

  // warning: check size of hashmap
  def f(solution: Solution[P]): Option[Double] = {
    // buffer performance values in hashmap
    solutions.get(hashSolution(solution)) match {
      case Some(value) => solutions.get(hashSolution(solution)).get
      case _ => {
        val performanceValue = runner.execute(solution).performance
        solutions.+=(hashSolution(solution) -> performanceValue)
        performanceValue
      }
    }
  }

  override def importSolution(filename: String): Solution[P] = {
    importExport match {
      case None => throw new Exception("don't know how to read a solution from disk")
      case Some(function) =>
        function._1.apply(filename)
    }
  }

  override def exportSolution(solution: Solution[P], filename: String): Unit = {
    importExport match {
      case None => throw new Exception("don't know how to write a solution to disk")
      case Some(function) => function._2.apply(solution, filename)
    }
  }
}

