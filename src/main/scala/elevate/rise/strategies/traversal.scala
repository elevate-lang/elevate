package elevate.rise.strategies

import elevate.core._
import rise.core.primitives._
import elevate.rise.rules.algorithmic._
import elevate.core.strategies.traversal._
import elevate.core.strategies.basic._
import elevate.rise.Rise
import elevate.rise.rules.traversal._
import elevate.rise.strategies.algorithmic._
import elevate.rise.strategies.normalForm._
import elevate.rise.strategies.predicate.isReduce

object traversal {

  // applying a strategy to an expression applied to a lift `map`. Example:
  // ((map λe14. (transpose ((map (map e12)) e14))) e13) // input expr
  //  (map λe14. (transpose ((map (map e12)) e14)))      // result of `function`
  //       λe14. (transpose ((map (map e12)) e14))       // result of `argument`
  //             (transpose ((map (map e12)) e14))       // result of 'body' -> here we can apply s
  def fmap: Strategy[Rise] => Strategy[Rise] = s => function(argumentOf(Map()(), body(s)))

  // fmap applied for expressions in rewrite normal form:
  // fuse -> fmap -> fission
  def fmapRNF: Strategy[Rise] => Strategy[Rise] =
    s => DFNF `;` mapFusion `;`
         DFNF `;` fmap(s) `;`
         DFNF `;` one(mapFullFission)

  // applying a strategy to an expression nested in one or multiple lift `map`s
  def mapped: Strategy[Rise] => Strategy[Rise] =
    s => s <+ (e => fmapRNF(mapped(s))(e))

  // moves along RNF-normalized expression
  // e.g., expr == ***f o ****g o *h
  // move(0)(s) == s(***f o ****g o *h)
  // move(1)(s) == s(****g o *h)
  // move(2)(s) == s(*h)
  def moveTowardsArgument: Int => Strategy[Rise] => Strategy[Rise] =
    i => s => applyNTimes(i)((e: Strategy[Rise]) => argument(e))(s)

  // TRAVERSAL DSL as described in ICFP'20 /////////////////////////////////////
  type Traversal[P] = Strategy[P] => Strategy[P]

  implicit class AtHelper[P](s: Strategy[P]) {
    def at(traversal: Traversal[P]): Strategy[P] =
      traversal(s)
    def `@`(traversal: Traversal[P]): Strategy[P] = // scalastyle:ignore
      traversal(s)
  }


  def outermostTraversal: Traversal[Rise] => Traversal[Rise] = {
    traversal => s => topDown(traversal(s))
  }

  def outermost: Strategy[Rise] => Strategy[Rise] => Strategy[Rise] = {
    predicate => s => topDown(predicate `;` s)
  }

  def innermost: Strategy[Rise] => Strategy[Rise] => Strategy[Rise] = {
    predicate => s => bottomUp(predicate `;` s)
  }

  def check: Strategy[Rise] => Traversal[Rise] = {
    predicate => predicate `;` _
  }

  def mapNest(d: Int): Strategy[Rise] = p => (d match {
    case x if x == 0 => Success(p)
    case x if x < 0  => Failure(mapNest(d))
    case _ => fmap(mapNest(d-1))(p)
  })

  def blocking: Strategy[Rise] = {
    id[Rise]() `@` outermost(mapNest(2))
    id[Rise]() `@` outermost(isReduce)
  }
}
