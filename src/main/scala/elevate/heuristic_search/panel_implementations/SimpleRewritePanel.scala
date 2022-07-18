package elevate.heuristic_search.panel_implementations

import elevate.core.{Failure, Strategy, Success}
import elevate.heuristic_search.{HeuristicPanel, Runner}
import elevate.heuristic_search.util.{Solution, hashProgram}

// do we need elevate strategies here? No!

// todo implement class
class SimpleRewritePanel[P](
                             val runner: Runner[P],
                             val strategies: Seq[Strategy[P]],
                             val afterRewrite: Option[Strategy[P]] = None, // e.g. rewrite normal form
                             val beforeExecution: Option[Strategy[P]] = None, // e.g. code-gen normal form
                             val rewriter: Option[Solution[P] => Seq[Solution[P]]] = None,
                             val importExport: Option[(String => Solution[P], (Solution[P], String) => Unit)]
                           ) extends HeuristicPanel[P] {


  val solutions = new scala.collection.mutable.HashMap[String, Option[Double]]()

  //  class StandardPanel[P](
  //                          val runner: Runner[P],
  //                          val strategies: Set[Strategy[P]],
  //                          val afterRewrite: Option[Strategy[P]] = None, // e.g. rewrite normal form
  //                          val beforeExecution: Option[Strategy[P]] = None, // e.g. code-gen normal form
  //                          val rewriter: Option[Solution[P] => Set[Solution[P]]] = None,
  //                          val importExport: Option[(String => Solution[P], (Solution[P], String) => Unit)]
  //                        ) extends HeuristicPanel[P] {

  override def N(solution: Solution[P]): Seq[Solution[P]] = {

    rewriter match {
      // expand strategy mode
      case Some(rewriteFunction) =>

        val result: Seq[Solution[P]] = afterRewrite match {
          case Some(aftermath) =>
            // todo check if normal form can be applied always
            rewriteFunction.apply(solution).map(elem => Solution(aftermath.apply(elem.expression).get, elem.strategies)).filter(runner.checkSolution)
          //            rewriteFunction.apply(solution).map(elem => Solution(aftermath.apply(elem.expression).get, elem.strategies))
          case None =>
            rewriteFunction.apply(solution)
        }

        result

      // default mode
      case None => N_default(solution)
    }
  }

  def N_default(solution: Solution[P]): Seq[Solution[P]] = {

    val NsOptions = strategies.map(strategy => {
      //      val NsOptions  = strategies.par.map(strategy => {
      try {

        val result = strategy.apply(solution.expression)

        //        this.synchronized {

        result match {
          case _: Success[P] => Some(new Solution[P](result.get, solution.strategies :+ strategy)).filter(runner.checkSolution)
          case _: Failure[P] =>
            //              println("failure: " + result.toString)
            None
        }
        //        }
      } catch {
        case e: Throwable => None
      }
    })
    //    val Ns = NsOptions.seq.flatten
    val Ns = NsOptions.flatten

    Ns
  }

  // warning: check size of hashmap
  override def f(solution: Solution[P]): Option[Double] = {
    // buffer performance values in hashmap
    solutions.get(hashProgram(solution.expression)) match {
      case Some(value) => solutions.get(hashProgram(solution.expression)).get
      case _ => {
        val performanceValue = runner.execute(solution).performance
        solutions.+=(hashProgram(solution.expression) -> performanceValue)
        performanceValue
      }
    }
  }

  override def getSolution(solution: Solution[P], numbers: Seq[Int]): Option[Solution[P]] = {
    // what to return?
    // we don't have a default constructor for the solutions?
    // todo doesn't work
    Some(Solution(solution.expression, solution.strategies))
  }

  // we don't expect invalid solutions in this search space
  override def checkRewrite(solution: Solution[P], rewrite: Int): Boolean = {
    true
  }

  override def importSolution(filename: String): Solution[P] = ???

  override def exportSolution(solution: Solution[P], filename: String): Unit = ???

}
