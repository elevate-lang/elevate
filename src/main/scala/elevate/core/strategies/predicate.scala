package elevate.core.strategies

import elevate.core._
import elevate.core.strategies.traversal.topDown
import elevate.core.macros._

import scala.language.implicitConversions

object predicate:

  implicit def rewriteResultToBoolean[P](r: RewriteResult[P]): Boolean =
    r match
      case Failure(_) => false
      case Success(_) => true
  
  def not[P](s: Strategy[P]): Strategy[P] =
    strategy("not", e => s(e) match
        case Success(_) => Failure(not(s))
        case Failure(_) => Success(e))

  // only fails if the else case fails
  // id is required to not trigger the else case if the then case fails
  def ifThenElse[P](p: Strategy[P]): Strategy[P] => Strategy[P] => Strategy[P] =
    combinator("ifThenElse", t => e => (p `;` (t <+ basic.id)) <+ e)

  def `if`[P]: Strategy[P] => Strategy[P] => Strategy[P] = p => t =>
    ifThenElse(p)(t)(basic.id)

  def ![P](s: Strategy[P]): Strategy[P] = not(s)
  
  def isEqualTo[P](x: P): Strategy[P] =
    strategy("isEqualTo", p => if p == x then Success(p) else Failure(isEqualTo(x)))
  
  def contains[P: Traversable](x: P): Strategy[P] =
    strategy(s"contains($x)", p => topDown(isEqualTo(x)).apply(p))
  
  def liftPredicate[P](f: P => Boolean): Strategy[P] =
    strategy("liftPredicate", p => if f(p) then Success(p) else Failure(liftPredicate(f)))

end predicate
