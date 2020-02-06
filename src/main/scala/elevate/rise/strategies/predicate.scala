package elevate.rise.strategies

import elevate.core.{Failure, RewriteResult, Strategy, Success}
import elevate.rise.Rise
import rise.core.primitives.{Generate, Let, MakeArray, Map, Reduce, Zip}
import rise.core.{App, DepLambda, Identifier, Lambda}

object predicate {

  // Matching Single Nodes

  case object isLambda extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = e match {
      case l: Lambda => Success(l)
      case _         => Failure(isLambda)
    }
    override def toString: String = "isLambda"
  }

  case object isDepLambda extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = e match {
      case l: DepLambda[_] => Success(l)
      case _               => Failure(isDepLambda)
    }
    override def toString: String = "isDepLambda"
  }

  case object isMakeArray extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = e match {
      case m: MakeArray => Success(m)
      case _            => Failure(isMakeArray)
    }
    override def toString: String = "isMakeArray"
  }

  case object isIdentifier extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = e match {
      case i: Identifier => Success[Rise](i)
      case _             => Failure[Rise](isIdentifier)
    }
    override def toString: String = "isIdentifier"
  }

  case object isReduce extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = e match {
      case r@Reduce() => Success(r)
      case _          => Failure(isMap)
    }
    override def toString: String = "isReduce"
  }

  case object isGenerate extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = e match {
      case g@Generate() => Success(g)
      case _            => Failure(isGenerate)
    }
    override def toString: String = "isGenerate"
  }

  case object isApply extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = e match {
      case a:App => Success(a)
      case _     => Failure(isApply)
    }
    override def toString: String = "isApply"
  }

  case object isMap extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = e match {
      case m@Map() => Success(m)
      case _       => Failure(isMap)
    }
    override def toString: String = "isMap"
  }

  // Matching Applied Primitives

  case class isApplied(s: Strategy[Rise]) extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = e match {
      case App(f, _) => s(f)
      case _         => Failure(isAppliedLet)
    }
    override def toString: String = "isAppliedLet"
  }

  case object isAppliedLet extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = e match {
      case a@App(App(Let(), _), _) => Success(a)
      case _                       => Failure(isAppliedLet)
    }
    override def toString: String = "isAppliedLet"
  }

  case object isAppliedMap extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = e match {
      case m@App(App(Map(), f), arg) => Success(m)
      case _                         => Failure(isMap)
    }
    override def toString: String = "isAppliedMap"
  }

  case object isAppliedZip extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = e match {
      case z@App(App(Zip(), a), b) => Success(z)
      case _                       => Failure(isAppliedZip)
    }
    override def toString: String = "isAppliedZip"
  }

  case object isAppliedReduce extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = e match {
      case r@App(App(App(Reduce(), op), init), arg) => Success(r)
      case _                                        => Failure(isMap)
    }
    override def toString: String = "isAppliedReduce"
  }
}
