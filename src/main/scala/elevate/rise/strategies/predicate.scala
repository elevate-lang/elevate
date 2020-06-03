package elevate.rise.strategies

import elevate.core.{Failure, RewriteResult, Strategy, Success}
import elevate.core.strategies.predicate._
import elevate.rise._
import elevate.rise.rules.lowering.isComputation
import rise.core.primitives.{Generate, Let, Map, Reduce, ReduceSeq, Transpose, Zip}
import rise.core.types._
import rise.core.{App, Identifier, Lambda}

object predicate {

  // Matching Single Nodes

  case object isLambda extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = e match {
      case l: Lambda => Success(l)
      case _         => Failure(isLambda)
    }
    override def toString = "isLambda"
  }

  case object isIdentifier extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = e match {
      case i: Identifier => Success[Rise](i)
      case _             => Failure[Rise](isIdentifier)
    }
    override def toString = "isIdentifier"
  }

  case object isReduce extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = e match {
      case r@Reduce() => Success(r)
      case _          => Failure(isReduce)
    }
    override def toString = "isReduce"
  }

  case object isTranspose extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = e match {
      case t@Transpose() => Success(t)
      case _             => Failure(isTranspose)
    }
    override def toString = "isTranspose"
  }

  case object isReduceSeq extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = e match {
      case r@ReduceSeq() => Success(r)
      case _             => Failure(isReduceSeq)
    }
    override def toString = "isReduce"
  }

  def isReduceX: Strategy[Rise] = (isReduce <+ isReduceSeq)

  case object isArray extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = e match {
      case e :: ArrayType(_,_) => Success(e)
      case _ => Failure(isArray)
    }
  }

  case object isGenerate extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = e match {
      case g@Generate() => Success(g)
      case _            => Failure(isGenerate)
    }
    override def toString = "isGenerate"
  }

  case object isApply extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = e match {
      case a:App => Success(a)
      case _     => Failure(isApply)
    }
    override def toString = "isApply"
  }

  case object isMap extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = e match {
      case m@Map() => Success(m)
      case _       => Failure(isMap)
    }
    override def toString = "isMap"
  }

  case object isLet extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = e match {
      case l@Let() => Success(l)
      case _       => Failure(isLet)
    }
    override def toString = "isLet"
  }

  // Matching Applied Primitives

  case object isAppliedLet extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = e match {
      case a@App(App(Let(), _), _) => Success(a)
      case _                       => Failure(isAppliedLet)
    }
    override def toString = "isAppliedLet"
  }

  case object isAppliedMap extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = e match {
      case m@App(App(Map(), f), arg) => Success(m)
      case _                         => Failure(isMap)
    }
    override def toString = "isAppliedMap"
  }

  case object isAppliedZip extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = e match {
      case z@App(App(Zip(), a), b) => Success(z)
      case _                       => Failure(isAppliedZip)
    }
    override def toString = "isAppliedZip"
  }

  case object isAppliedReduce extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = e match {
      case r@App(App(App(Reduce(), op), init), arg) => Success(r)
      case _                                        => Failure(isMap)
    }
    override def toString = "isAppliedReduce"
  }

  case class isApplied(s: Strategy[Rise]) extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = e match {
      case a@App(f,e) => s(f).mapSuccess(_ => a)
      case _          => Failure(isApplied(s))
    }
  }

  case object isVectorizeablePrimitive extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = e match {
      case a@App(App(Map(), f), input) if isComputation(f) && !isVectorArray(a.t) => Success(a)
      case r@App(App(App(Reduce(), op), init), input) if isComputation(op) => Success(r)
      case _ => Failure(isVectorizeablePrimitive)
    }

    def isVectorArray(t: Type): Boolean = t match {
      case ArrayType(_, VectorType(_,_)) => true
      case _ => false
    }
  }
}
