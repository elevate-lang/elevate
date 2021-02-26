package elevate.core.strategies

import elevate.core.RewriteResult._
import elevate.core._
import elevate.core.strategies.basic._

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
object traversal:

  // generic one-level traversal strategies

  case class all[P: Traversable](s: Strategy[P]) extends Strategy[P]:
    def apply(p: P): RewriteResult[P] = summon[Traversable[P]].all(s)(p)

  case class one[P: Traversable](s: Strategy[P]) extends Strategy[P]:
    def apply(p: P): RewriteResult[P] = summon[Traversable[P]].one(s)(p)

  case class some[P: Traversable](s: Strategy[P]) extends Strategy[P]:
    def apply(p: P): RewriteResult[P] = summon[Traversable[P]].some(s)(p)

  case class oneUsingState[P: Traversable](s: Strategy[P]) extends Strategy[P]:
    def apply(p: P): RewriteResult[P] = summon[Traversable[P]].oneUsingState(s)(p)

  // generic complete traversal strategies

  case class topDown[P: Traversable](s: Strategy[P]) extends Strategy[P]:
    def apply(p: P): RewriteResult[P] = (s <+ one(topDown(s))) (p)

  case class allTopdown[P: Traversable](s: Strategy[P]) extends Strategy[P]:
    def apply(p: P): RewriteResult[P] = (s `;` all(allTopdown(s))) (p)

  case class tryAll[P: Traversable](s: Strategy[P]) extends Strategy[P]:
    def apply(p: P): RewriteResult[P] = (all(tryAll(`try`(s))) `;` `try`(s)) (p)

  case class allBottomup[P: Traversable](s: Strategy[P]) extends Strategy[P]:
    def apply(p: P): RewriteResult[P] = (all(allBottomup(s)) `;` s) (p)

  case class downup[P: Traversable](s: Strategy[P]) extends Strategy[P]:
    def apply(p: P): RewriteResult[P] = (s `;` (all(downup(s)) `;` s)) (p)

  case class downup2[P: Traversable](s1: Strategy[P], s2: Strategy[P]) extends Strategy[P]:
    def apply(p: P): RewriteResult[P] = (s1 `;` (all(downup2(s1, s2)) `;` s2)) (p)

  case class bottomUp[P: Traversable](s: Strategy[P]) extends Strategy[P]:
    def apply(p: P): RewriteResult[P] = (one(bottomUp(s)) <+ s)(p)

  case class alltd[P: Traversable](s: Strategy[P]) extends Strategy[P]:
    def apply(p: P): RewriteResult[P] = (s <+ all(alltd(s)))(p)

  case class sometd[P: Traversable](s: Strategy[P]) extends Strategy[P]:
    def apply(p: P): RewriteResult[P] = (s <+ some(sometd(s)))(p)

  case class somebu[P: Traversable](s: Strategy[P]) extends Strategy[P]:
    def apply(p: P): RewriteResult[P] = (some(somebu(s)) <+ s)(p)

  // counting traversal strategies describing absolute positions

  case class position[P: Traversable](n: Int)(s: Strategy[P]) extends Strategy[P]:
    def apply(p: P): RewriteResult[P] = if n <= 0 then s(p) else oneUsingState(position(n - 1)(s)).apply(p)

  case class skip[P: Traversable](n: Int)(s: Strategy[P]) extends Strategy[P]:
    def apply(p: P): RewriteResult[P] = s(p) match
      case Failure(a)           => oneUsingState(skip(n)(a)).apply(p)
      case Success(_) if n > 0  => oneUsingState(skip(n - 1)(s)).apply(p)
      case Success(r)           => Success(r)

end traversal
