package elevate.core.strategies

import elevate.core._
import elevate.core.strategies.traversal.topDown
import elevate.macros.CombinatorMacro.combinator
import elevate.macros.StrategyMacro.strategy

import scala.language.implicitConversions

object predicate {

  implicit def rewriteResultToBoolean[P](r: RewriteResult[P]): Boolean =
    r match {
      case Failure(_) => false
      case Success(_) => true
    }

  @combinator
  def not[P]: Strategy[P] => Strategy[P] = s => e => s(e) match {
    case Success(_) => Failure(not(s))
    case Failure(_) => Success(e)
  }

  // only fails if the else case fails
  // id is required to not trigger the else case if the then case fails
  @combinator
  def ifThenElse[P]: Strategy[P] => Strategy[P] => Strategy[P] => Strategy[P] =
    p => t => e => (p `;` (t <+ basic.id)) <+ e

  def `if`[P]: Strategy[P] => Strategy[P] => Strategy[P] = p => t =>
    ifThenElse(p)(t)(basic.id)

  def ![P](s: Strategy[P]): Strategy[P] = not(s)

  @strategy
  def isEqualTo[P](x: P): Strategy[P] = p => if (p == x) Success(p) else Failure(isEqualTo(x))

  case class contains[P: Traversable](x: P) extends Strategy[P] {
    def apply(p: P): RewriteResult[P] = topDown(isEqualTo(x)).apply(p)
    override def toString: String = s"contains($x)"
  }

  @strategy
  def liftPredicate[P](f: P => Boolean): Strategy[P] = p => if (f(p)) Success(p) else Failure(liftPredicate(f))
}
