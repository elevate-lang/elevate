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