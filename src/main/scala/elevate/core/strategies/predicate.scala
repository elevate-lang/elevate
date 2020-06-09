package elevate.core.strategies

import elevate.core._
import elevate.core.strategies.traversal.topDown

import scala.language.implicitConversions

object predicate {

  implicit def rewriteResultToBoolean[P](r: RewriteResult[P]): Boolean =
    r match {
      case Failure(_) => false
      case Success(_) => true
    }

  case class not[P](s: Strategy[P]) extends Strategy[P] {
    def apply(e: P): RewriteResult[P] = s(e) match {
      case Success(_) => Failure(not(s))
      case Failure(_) => Success(e)
    }
    override def toString: String = s"not($s)"
  }

  // only fails if the else case fails
  // id is required to not trigger the else case if the then case fails
  def ifThenElse[P](p: Strategy[P],
                    t: Strategy[P],
                    e: Strategy[P]): Strategy[P] =
    (p `;` (t <+ basic.id)) <+ e

  def `if`[P]: Strategy[P] => Strategy[P] => Strategy[P] = p => t =>
    ifThenElse(p,t,basic.id)

  def ![P](s: Strategy[P]): Strategy[P] = not(s)

  case class isEqualTo[P](x: P) extends Strategy[P] {
    def apply(p: P): RewriteResult[P] =
      if (p == x) Success(p) else Failure(isEqualTo(x))
    override def toString: String = s"isEqualTo($x)"
  }

  case class contains[P: Traversable](x: P) extends Strategy[P] {
    def apply(p: P): RewriteResult[P] = topDown(isEqualTo(x)).apply(p)
    override def toString: String = s"contains($x)"
  }

  case class liftPredicate[P](f: P => Boolean) extends Strategy[P] {
    def apply(p: P): RewriteResult[P] =
      if (f(p)) Success(p) else Failure(liftPredicate(f))
    override def toString = s"liftPredicate($f)"
  }
}
