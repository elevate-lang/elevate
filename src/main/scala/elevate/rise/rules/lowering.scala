package elevate.rise.rules

import elevate.core.strategies.traversal.oncetd
import elevate.core.strategies.predicate._
import elevate.rise.rules.traversal._
import elevate.core.{Failure, RewriteResult, Strategy, Success}
import elevate.rise.Rise
// import elevate.rise.strategies.predicate.isMakeArray
import rise.core._
import rise.core.primitives._
import rise.core.TypedDSL._
import rise.core.types._

object lowering {

  // Straight-forward Lowering

  case object mapSeq extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = e match {
      case m@Map() => Success(MapSeq()(m.t) :: e.t)
      case _       => Failure(mapSeq)
    }
    override def toString: String = "mapSeq"
  }

  case object mapSeqUnroll extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = e match {
      case m@Map() => Success(MapSeqUnroll()(m.t) :: e.t)
      case _       => Failure(mapSeqUnroll)
    }
    override def toString: String = "mapSeqUnroll"
  }

  case class mapGlobal(dim: Int = 0) extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = e match {
      case Map() => Success(rise.OpenCL.TypedDSL.mapGlobal(dim) :: e.t)
      case _       => Failure(mapGlobal(dim))
    }
    override def toString: String = "mapGlobal"
  }

  case object reduceSeq extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = e match {
      case Reduce() => Success(TypedDSL.reduceSeq :: e.t)
      case _        => Failure(reduceSeq)
    }
    override def toString: String = "reduceSeq"
  }

  // todo shall we allow lowering from an already lowered reduceSeq?
  case object reduceSeqUnroll extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = e match {
      case Reduce() | ReduceSeq() => Success(TypedDSL.reduceSeqUnroll :: e.t)
      case _                      => Failure(reduceSeqUnroll)
    }
    override def toString: String = "reduceSeqUnroll"
  }

  // Specialized Lowering

  case object mapSeqCompute extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = e match {
      // in the old branch we also checked that it does not map zips
      case App(Map(), f)
        //if (containsComputation `;` not(oncetd(isMakeArray)))(f) =>
        if (containsComputation)(f) =>
        Success(app(TypedDSL.mapSeq, f))
      case _ => Failure(mapSeqCompute)
    }
  }

  def containsComputation: Strategy[Rise] = oncetd(isComputation)

  case object isComputation extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = e match {
      // unary function (map)
      case l@Lambda(_,_) =>
        l.t match {
          case FunType(in, out)
            if isPairOrBasicType(in) && isPairOrBasicType(out) => Success(l)
          case _ => Failure(isComputation)
        }
      // binary function (reduce)
      // case l@Lambda(_,_) :: FunType(in, FunType(in2, out)) if
      // isPairOrBasicType(in) &&
      // isPairOrBasicType(in2) && isPairOrBasicType(out) => Success(l)
      // case l@Lambda(_,_) if isId(l) => Success(l)
      // case reduceX(r) => Success(r)
      case f@ForeignFunction(_) => Success(f)
      case _ => Failure(containsComputation)
    }

    private def isPairOrBasicType(t: Type): Boolean = t match {
      case _: BasicType => true
      case PairType(a,b) => isPairOrBasicType(a) && isPairOrBasicType(b)
      case _ => false
    }
  }

  case class slideSeq(rot: SlideSeq.Rotate,
                      write_dt1: Expr) extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = e match {
      case Slide() => Success(nFun(sz => nFun(sp =>
        TypedDSL.slideSeq(rot)(sz)(sp)(untyped(write_dt1))(fun(x => x))
      )) :: e.t)
      case _ => Failure(slideSeq(rot, write_dt1))
    }
    override def toString: String = s"slideSeq($rot, $write_dt1)"
  }
}