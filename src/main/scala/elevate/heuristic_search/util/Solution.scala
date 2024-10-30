package elevate.heuristic_search.util

import elevate.core.Strategy

import scala.collection.immutable

//case class Solution[P](expression: P, strategies: immutable.Seq[Strategy[P]])

case class Solution[P](
                        solutionSteps: Seq[SolutionStep[P]]
                      ) {
  def expression(): P = {
    solutionSteps.last.expression
  }

  def strategies(): Seq[Strategy[P]] = {
    solutionSteps.map(step => step.strategy)
  }

  def rewrites(): Seq[RewriteIdentifier[P]] = {
    solutionSteps.map(step =>
      RewriteIdentifier[P](
        strategy = step.strategy,
        location = step.location
      )
    )
  }

  def rewrite_sequence(): Seq[(Strategy[P], Int)] = {
    solutionSteps.map(step => (step.strategy, step.location))
  }

  def parent(): Solution[P] = {
    Solution[P](solutionSteps.dropRight(1))
  }

  def layer(): Int = {
    solutionSteps.size
  }

  override def toString: String = {
    solutionSteps.map(step => s"${step.strategy}, ${step.location}").mkString("\n")
  }

}

case class RewriteIdentifier[P](
                                 strategy: Strategy[P],
                                 location: Int
                               ) {
  override def toString: String = {
    s"(${strategy}, ${location})"
  }
}

case class SolutionStep[P](
                            expression: P,
                            strategy: Strategy[P],
                            location: Int
                          )

//class Solution2[P](elements: Seq[SolutionStep[P]]) {
//
//   // we could do this for compatibility reasons
//  def strategies(): Seq[Strategy[P]] = {
//    elements.map(elem => elem.strategy)
//  }
//
//}