package elevate.core.strategies

import elevate.core._
import elevate.core.strategies.traversal.topDown
import elevate.core.macros._

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
  def id[P]: Strategy[P] = strategy("id", p => Success(p))
  
  def fail[P]: Strategy[P] = strategy("fail", _ => Failure(fail))

  // Basic Strategy Combinators
  def seq[P]: Strategy[P] => Strategy[P] => Strategy[P] =
    combinator("seq", fs => ss => p => fs(p).flatMapSuccess(ss))
  
  def leftChoice[P]: Strategy[P] => Strategy[P] => Strategy[P] =
    combinator("leftChoice", fs => ss => p => fs(p).flatMapFailure(_ => ss(p)))

  // Basic Strategies
  def `try`[P](s: Strategy[P]): Strategy[P] =
    strategy("try", s <+ id)
  
  def repeat[P](s: Strategy[P]): Strategy[P] =
    strategy("repeat", `try`(s `;` repeat(s)))
  
  def repeatNTimes[P](n: Int)(s: Strategy[P]): Strategy[P] =
    strategy("repeatNTimes", p => if (n > 0) {(s `;` repeatNTimes(n - 1)(s))(p)} else { id(p) })

  // Normalize
  def normalize[P: Traversable](s: Strategy[P]): Strategy[P] =
     strategy("normalize", repeat(topDown.apply(s)))

  // Strategy Factories
  def applyNTimes[P](i: Int)(f: (Strategy[P] => Strategy[P]))(s: Strategy[P]): Strategy[P] =
    strategy("applyNTimes", if(i <= 0) s else applyNTimes[P](i-1)(f)(f(s)))
}
