package elevate.core.strategies

import elevate.core._
import elevate.core.strategies.traversal.topDown
import elevate.macros.CombinatorMacro.combinator
import elevate.macros.RuleMacro.rule

/* Inspired by:

@inproceedings{DBLP:conf/icfp/VisserBT98,
  author    = {Eelco Visser and
               Zine{-}El{-}Abidine Benaissa and
               Andrew P. Tolmach},
  title     = {Building Program Optimizers with Rewriting Strategies},
  booktitle = {{ICFP}},
  pages     = {13--26},
  publisher = {{ACM}},
  year      = {1998}
}

 */
object basic {

  // Naive Strategies

  @rule def id[P]: Strategy[P] = p => Success(p)

  @rule def fail[P]: Strategy[P] = _ => Failure(fail)

  // Basic Strategy Combinators

  @combinator
  def seq[P]: Strategy[P] => Strategy[P] => Strategy[P] =
    fs => ss => p => fs(p).flatMapSuccess(ss)

  @combinator
  def leftChoice[P]: Strategy[P] => Strategy[P] => Strategy[P] =
    fs => ss => p => fs(p).flatMapFailure(_ => ss(p))

  // Basic Strategies

  @combinator
  def `try`[P]: Strategy[P] => Strategy[P] = s => s <+ id

  @combinator
  def repeat[P]: Strategy[P] => Strategy[P] = s => `try`(s `;` repeat(s))

  @combinator
  def repeatNTimes[P](n: Int): Strategy[P] => Strategy[P] = s =>
    p => if (n > 0) {(s `;` repeatNTimes(n - 1)(s))(p)} else { id(p) }

  // Normalize

  @combinator
  def normalize[P: Traversable]: Strategy[P] => Strategy[P] =
    s => repeat(topDown.apply(s))

  // Strategy Factories

  def applyNTimes[P]: Int => (Strategy[P] => Strategy[P]) => Strategy[P] => Strategy[P] =
    i => f => s => if(i <= 0) s else applyNTimes[P](i-1)(f)(f(s))
}
