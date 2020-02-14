package elevate.rise.rules

import elevate.core.strategies.basic._
import elevate.core.strategies.predicate._
import elevate.core.strategies.traversal.{oncetd, tryAll}
import elevate.core.{Failure, RewriteResult, Strategy, Success}
import elevate.rise.{ReduceX, Rise}
import elevate.rise.rules.traversal._
import elevate.rise.strategies.normalForm.LCNF
import elevate.rise.strategies.predicate._
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
    override def toString = "mapSeq"
  }

  case object mapSeqUnroll extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = e match {
      case m@Map() => Success(MapSeqUnroll()(m.t) :: e.t)
      case _       => Failure(mapSeqUnroll)
    }
    override def toString = "mapSeqUnroll"
  }

  case class mapGlobal(dim: Int = 0) extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = e match {
      case Map() => Success(rise.OpenCL.TypedDSL.mapGlobal(dim) :: e.t)
      case _       => Failure(mapGlobal(dim))
    }
    override def toString = "mapGlobal"
  }

  case object reduceSeq extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = e match {
      case Reduce() => Success(TypedDSL.reduceSeq :: e.t)
      case _        => Failure(reduceSeq)
    }
    override def toString = "reduceSeq"
  }

  // todo shall we allow lowering from an already lowered reduceSeq?
  case object reduceSeqUnroll extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = e match {
      case Reduce() | ReduceSeq() => Success(TypedDSL.reduceSeqUnroll :: e.t)
      case _                      => Failure(reduceSeqUnroll)
    }
    override def toString = "reduceSeqUnroll"
  }

  // Specialized Lowering

  // only transforms maps which contain ForeignFunctions or mapSeqs
//  case object mapSeqCompute extends Strategy[Rise] {
//    def apply(e: Rise): RewriteResult[Rise] = e match {
//      // (mapSeq λη1. (my_abs η1))
//      case App(m @ Map(), l @ Lambda(_, App(ForeignFunction(_), _))) => Success(app(TypedDSL.mapSeq :: m.t, l) :: e.t)
//      // (map λη1. ((mapSeq λη2. (my_abs η2)) η1))
//      case App(m @ Map(), l @ Lambda(_, App(App(MapSeq(), _), _))) => Success(app(TypedDSL.mapSeq :: m.t, l) :: e.t)
//      case _ => Failure(mapSeqCompute)
//    }
//    override def toString = "mapSeqCompute"
//  }

  case object mapSeqCompute extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = e match {
      case App(Map(), f) if containsComputation(f) && not(isMappingZip)(f) =>
        Success(TypedDSL.mapSeq(f))
      case _ => Failure(mapSeqCompute)
    }
  }

  case object isMappingZip extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = e match {
      case l@Lambda(_, App(App(Zip(), a), b)) => Success(l)
      case m@Lambda(_, App(App(Map(), f), arg)) => isMappingZip(f)
      case _ => Failure(isMappingZip)
    }
  }

  def containsComputation: Strategy[Rise] = oncetd(isComputation)

  // requires type information!
  case object isComputation extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = e match {
      // unary function (map)
      case l@Lambda(_,_) if isId(l) => Success(l)
      case l@Lambda(_,_) =>
        l.t match {
          // unary function
          case FunType(in, out) if
            isPairOrBasicType(in) && isPairOrBasicType(out) => Success(l)
          // binary function
          case FunType(in, FunType(in2, out)) if
            isPairOrBasicType(in) && isPairOrBasicType(in2) &&
            isPairOrBasicType(out) => Success(l)
          case _ => Failure(containsComputation)
        }
      case f@ForeignFunction(_) => Success(f)
      case _ => Failure(containsComputation)
    }

    private def isPairOrBasicType(t: Type): Boolean = t match {
      case _:BasicType => true
      case PairType(a,b) => isPairOrBasicType(a) && isPairOrBasicType(b)
      case _ => false
    }
  }


  case class slideSeq(rot: SlideSeq.Rotate, write_dt1: Expr) extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = e match {
      case Slide() => Success(nFun(sz => nFun(sp =>
        TypedDSL.slideSeq(rot)(sz)(sp)(untyped(write_dt1))(fun(x => x))
      )) :: e.t)
      case _ => Failure(slideSeq(rot, write_dt1))
    }
    override def toString = s"slideSeq($rot, $write_dt1)"
  }

  // Lowerings used in PLDI submission

  // adds copy after every generate
  val materializeGenerate: Strategy[Rise] =
    normalize.apply(
      argument(function(isGenerate)) `;`
      not(isCopy) `;`
      argument(copyAfterGenerate)
    )

  // adds explicit copies for every init value in reductions
  val materializeInitOfReduce: Strategy[Rise] =
    normalize.apply(
      function(function(isReduceX)) `;`
      argument(not(isCopy) `;` insertCopyAfter)
    )

  case object insertCopyAfter extends Strategy[Rise] {
    def constructCopy(t: Type): TDSL[Rise] = t match {
      case ArrayType(_, dt) => TypedDSL.mapSeq(fun(x => constructCopy(dt) $ x))
      case _: BasicType => fun(x => x)
      case _ => ??? // shouldn't happen?
    }

    def apply(e: Rise): RewriteResult[Rise] = e match {
      case a => Success(constructCopy(a.t) $ a)
    }
  }


  // todo currently only works for mapSeq
  case object isCopy extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = e match {
      case c@App(Let(), id) if isId(id) => Success(c)
      case c@App(App(MapSeq(), id), etaInput) if isId(id) => Success(c)
      case App(App(MapSeq(), Lambda(_, f)), etaInput) => isCopy(f)
      case c@App(id, _) if isId(id) => Success(c)
      case _ => Failure(isCopy)
    }
  }

  def isId: Strategy[Rise] = {
    case l@Lambda(x1, x2) if x1 == x2 => Success(l)
    case _ => Failure(isId)
  }

  // requires expr to be in LCNF
  val specializeSeq: Strategy[Rise] =
    normalize.apply(lowering.mapSeqCompute <+ lowering.reduceSeq)

  val addRequiredCopies: Strategy[Rise] =
    // `try`(oncetd(copyAfterReduce)) `;` LCNF `;` materializeInitOfReduce
    tryAll(copyAfterReduce) `;` LCNF `;` materializeInitOfReduce

  // todo gotta use a normalform for introducing copies! e.g., if we have two reduce primitives
  val lowerToC: Strategy[Rise] = addRequiredCopies `;` specializeSeq


  // todo currently only works for mapSeq
  case object copyAfterReduce extends Strategy[Rise] {
    def constructCopy(t: Type): TDSL[Rise] = t match {
      case _: BasicType => let(fun(x => x))
      case ArrayType(_, _: BasicType) => TypedDSL.mapSeq(fun(x => x))
      case ArrayType(_, a: ArrayType) => TypedDSL.mapSeq(fun(x => constructCopy(a) $ x))
      case _ => ??? // shouldn't happen?
    }

    def apply(e: Rise): RewriteResult[Rise] = e match {
      case reduceResult@App(App(App(ReduceX(), _),_),_) =>
        Success(constructCopy(reduceResult.t) $ reduceResult)
      case _ => Failure(copyAfterReduce)
    }
  }

  // todo currently only works for mapSeq
  case object copyAfterGenerate extends Strategy[Rise] {
    def constructCopy(t: Type): TDSL[Rise] = t match {
      case ArrayType(_, dt) => TypedDSL.mapSeq(fun(x => constructCopy(dt) $ x))
      case _:BasicType => fun(x => x)
      case _ => ??? // shouldn't happen?
    }

    def apply(e: Rise): RewriteResult[Rise] = e match {
      case a@App(Generate(), _) => Success(constructCopy(a.t) $ a)
      case _ => Failure(copyAfterGenerate)
    }
  }
}