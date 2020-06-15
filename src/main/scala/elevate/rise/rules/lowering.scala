package elevate.rise.rules

import elevate.core.strategies.basic._
import elevate.core.strategies.predicate._
import elevate.core.strategies.traversal._
import elevate.core.{Failure, RewriteResult, Strategy, Success}
import elevate.rise._
import elevate.rise.rules.traversal._
import elevate.rise.strategies.normalForm.DFNF
import elevate.rise.strategies.predicate._
import elevate.rise.strategies.predicate.isVectorArray
import rise.openMP.TypedDSL.mapPar
import rise.core._
import rise.core.primitives._
import rise.core.TypedDSL._
import rise.core.types._
import arithexpr.arithmetic.Cst
import elevate.core.strategies.Traversable

object lowering {

  // Straight-forward Lowering

  case object mapSeq extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = e match {
      case m@Map() => Success(MapSeq()(m.t) :: e.t)
      case _ => Failure(mapSeq)
    }
    override def toString: String = "mapSeq"
  }

  case object mapStream extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = e match {
      case m@Map() => Success(MapStream()(m.t) :: e.t)
      case _       => Failure(mapStream)
    }
    override def toString = "mapStream"
  }

  case object iterateStream extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = e match {
      case m@Map() => Success(IterateStream()(m.t) :: e.t)
      case _       => Failure(iterateStream)
    }
    override def toString = "iterateStream"
  }

  case object mapSeqUnroll extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = e match {
      case m@Map() => Success(MapSeqUnroll()(m.t) :: e.t)
      case _ => Failure(mapSeqUnroll)
    }
    override def toString: String = "mapSeqUnroll"
  }

  case class mapGlobal(dim: Int = 0) extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = e match {
      case Map() => Success(rise.openCL.TypedDSL.mapGlobal(dim) :: e.t)
      case _       => Failure(mapGlobal(dim))
    }
    override def toString: String = "mapGlobal"
  }

  case object reduceSeq extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = e match {
      case Reduce() => Success(TypedDSL.reduceSeq :: e.t)
      case _ => Failure(reduceSeq)
    }
    override def toString: String = "reduceSeq"
  }

  // todo shall we allow lowering from an already lowered reduceSeq?
  case object reduceSeqUnroll extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = e match {
      case ReduceX() => Success(TypedDSL.reduceSeqUnroll :: e.t)
      case _ => Failure(reduceSeqUnroll)
    }
    override def toString: String = "reduceSeqUnroll"
  }

  // Specialized Lowering

  case class mapSeqCompute()(implicit ev: Traversable[Rise])
    extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = e match {
      case App(Map(), f) if containsComputation(ev)(f) && not(isMappingZip)(f) =>
        Success(TypedDSL.mapSeq(f))
      case _ => Failure(mapSeqCompute())
    }
  }

  case object isMappingZip extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = e match {
      case l@Lambda(_, App(App(Zip(), a), b)) => Success(l)
      case m@Lambda(_, App(App(Map(), f), arg)) => isMappingZip(f)
      case _ => Failure(isMappingZip)
    }
  }

  // TODO: load identity instead, then change with other rules?
  case class circularBuffer(load: Expr) extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = e match {
      case DepApp(DepApp(Slide(), sz: Nat), Cst(1)) => Success(
        TypedDSL.circularBuffer(sz)(sz)(untyped(load)) :: e.t)
      case _ => Failure(circularBuffer(load))
    }
    override def toString = s"circularBuffer($load)"
  }

  case class rotateValues(write: Expr) extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = e match {
      case DepApp(DepApp(Slide(), sz: Nat), Cst(1)) => Success(
        TypedDSL.rotateValues(sz)(untyped(write)) :: e.t)
      case _ => Failure(rotateValues(write))
    }
    override def toString = s"rotateValues($write)"
  }

  def containsComputation(implicit ev: Traversable[Rise]): Strategy[Rise] =
    topDown(isComputation())(ev)

  // requires type information!
  case class isComputation()(implicit ev: Traversable[Rise]) extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = e match {
      // unary function (map)
      case l@Lambda(_, _) if isId(l) => Success(l)
      case l@Lambda(_, _) =>
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
      case _: BasicType => true
      case PairType(a, b) => isPairOrBasicType(a) && isPairOrBasicType(b)
      case _ => false
    }
  }


//  case class slideSeq(rot: SlideSeq.Rotate, write_dt1: Expr) extends Strategy[Rise] {
//    def apply(e: Rise): RewriteResult[Rise] = e match {
//      case Slide() => Success(nFun(sz => nFun(sp =>
//        TypedDSL.slideSeq(rot)(sz)(sp)(untyped(write_dt))
//      )) :: e.t)
//      case _ => Failure(slideSeq(rot, write_dt))
//    }
//    override def toString = s"slideSeq($rot, $write_dt)"
//  }

  // writing to memory

  // TODO: think about more complex cases
  case object mapSeqUnrollWrite extends Strategy[Rise] {
    import rise.core.types._
    def apply(e: Rise): RewriteResult[Rise] = e.t match {
      case ArrayType(_, _: BasicType) =>
        Success(app(TypedDSL.mapSeqUnroll(fun(x => x)), typed(e)) :: e.t)
      case _ =>
        Failure(mapSeqUnrollWrite)
    }
    override def toString: String = s"mapSeqUnrollWrite"
  }

  case object toMemAfterMapSeq extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] =
      e match {
        case a@App(App(MapSeq(), _), _) =>
          Success((typed(a) |> TypedDSL.toMem) :: a.t)
        case _ => Failure(toMemAfterMapSeq)
      }
    override def toString = "toMemAfterMapSeq"
  }

  // Lowerings used in PLDI submission

  // adds copy after every generate
  def materializeGenerate(implicit ev: Traversable[Rise]): Strategy[Rise] =
    normalize.apply(
      argument(function(isGenerate)) `;`
        not(isCopy) `;`
        argument(copyAfterGenerate)
    )

  // adds explicit copies for every init value in reductions
  def materializeInitOfReduce(implicit ev: Traversable[Rise]): Strategy[Rise] =
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
  def specializeSeq(implicit ev: Traversable[Rise]): Strategy[Rise] =
    normalize.apply(lowering.mapSeqCompute() <+ lowering.reduceSeq)

  def addRequiredCopies(implicit ev: Traversable[Rise]): Strategy[Rise] =
    // `try`(oncetd(copyAfterReduce)) `;` LCNF `;` materializeInitOfReduce
    tryAll(copyAfterReduce) `;` DFNF() `;` materializeInitOfReduce

  // todo gotta use a normalform for introducing copies! e.g., if we have two reduce primitives
  def lowerToC(implicit ev: Traversable[Rise]): Strategy[Rise] =
    addRequiredCopies `;` `try`(bottomUp(copyAfterReduceInit)) `;` specializeSeq


  // todo currently only works for mapSeq
  case object copyAfterReduce extends Strategy[Rise] {
    def constructCopy(t: Type): TDSL[Rise] = t match {
      case _: BasicType => let(fun(x => x))
      case ArrayType(_, _: BasicType) => TypedDSL.mapSeq(fun(x => x))
      case ArrayType(_, a: ArrayType) => TypedDSL.mapSeq(fun(x => constructCopy(a) $ x))
      case _ => ??? // shouldn't happen?
    }

    def apply(e: Rise): RewriteResult[Rise] = e match {
      case reduceResult@App(App(App(ReduceX(), _), _), _) =>
        Success(constructCopy(reduceResult.t) $ reduceResult)
      case _ => Failure(copyAfterReduce)
    }
  }

  case object copyAfterReduceInit extends Strategy[Rise] {
    def constructCopy(t: Type): TDSL[Rise] = t match {
      case _: BasicType => let(fun(x => x))
      case ArrayType(_, _: BasicType) => TypedDSL.mapSeq(fun(x => x))
      case ArrayType(_, a: ArrayType) => TypedDSL.mapSeq(fun(x => constructCopy(a) $ x))
      case x => println(x) ; ??? // shouldn't happen?
    }

    def apply(e: Rise): RewriteResult[Rise] = e match {
      case App(a@App(ReduceSeqUnroll(), _), init) =>
        Success(TDSL(a) $ (constructCopy(init.t) $ init))
      case _ => Failure(copyAfterReduce)
    }
  }

  // todo currently only works for mapSeq
  case object copyAfterGenerate extends Strategy[Rise] {
    def constructCopy(t: Type): TDSL[Rise] = t match {
      case ArrayType(_, dt) => TypedDSL.mapSeq(fun(x => constructCopy(dt) $ x))
      case _: BasicType => fun(x => x)
      case _ => ??? // shouldn't happen?
    }

    def apply(e: Rise): RewriteResult[Rise] = e match {
      case a@App(Generate(), _) => Success(constructCopy(a.t) $ a)
      case _ => Failure(copyAfterGenerate)
    }
  }

  case class vectorize(n: Nat)(implicit ev: Traversable[Rise])
    extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = e match {
      case a@App(App(Map(), f), input) if
      isComputation()(ev)(f) && !isVectorArray(a.t) =>

        val newF = untyped(f)
        Success(
          (asScalar o map(newF)) $ (vectorizeArrayBasedOnType(input.t) $ input)
        )
      case _ => Failure(vectorize(n))
    }

    private def vectorizeArrayBasedOnType(t: Type): TDSL[Rise] = {
      def generateUnZips(dt: Type): TDSL[Rise] = {
        dt match {
          case _: BasicType => asVectorAligned(n)
          case PairType(aT, bT) => fun(x =>
            zip((generateUnZips(aT) $ x._1))(generateUnZips(bT) $ x._2)) o unzip
          case x => println(x) ; ???
        }
      }

      t match {
        case ArrayType(_, dt) => generateUnZips(dt) // remove first array layer
        case _ => ??? // shouldnt happen
      }
    }
  }

  def untype: Strategy[Rise] = p => Success(p.setType(TypePlaceholder))

  case class parallel()(implicit ev: Traversable[Rise]) extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = e match {
      case App(Map(), f) if containsComputation(ev)(f) => Success(mapPar(f))
      case _ => Failure(parallel())
    }
    override def toString = "parallel"
  }

  case object unroll extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = e match {
      case ReduceSeq() => Success(TypedDSL.reduceSeqUnroll)
      case _ => Failure(unroll)
    }
  }

  object ocl {
    import rise.openCL.TypedDSL
    import rise.openCL.primitives._
    import rise.core.types.AddressSpace

    case class reduceSeqUnroll(a: AddressSpace) extends Strategy[Rise] {
      def apply(e: Rise): RewriteResult[Rise] = e match {
        case ReduceX() => Success(TypedDSL.oclReduceSeqUnroll(a) :: e.t)
        case _ => Failure(reduceSeqUnroll(a))
      }
      override def toString = "reduceSeqUnroll"
    }

    case class circularBuffer(a: AddressSpace)
      extends Strategy[Rise] {
      def apply(e: Rise): RewriteResult[Rise] = e match {
        case DepApp(DepApp(Slide(), n: Nat), Cst(1)) =>
          Success(
            TypedDSL.oclCircularBuffer(a)(n)(n)(fun(x => x))
            :: e.t)
        case _ => Failure(circularBuffer(a))
      }
      override def toString = s"circularBuffer($a)"
    }

    case object circularBufferLoadFusion extends Strategy[Rise] {
      def apply(e: Rise): RewriteResult[Rise] = e match {
        case App(App(
          cb @ DepApp(DepApp(DepApp(OclCircularBuffer(), _), _), _),
          load), App(App(Map(), f), in)
        ) =>
          Success(untyped(cb)(typed(f) >> load, in) :: e.t)
        case _ => Failure(circularBufferLoadFusion)
      }
      override def toString = s"circularBufferLoadFusion"
    }

    case class rotateValues(a: AddressSpace, write: Expr)
      extends Strategy[Rise] {
      def apply(e: Rise): RewriteResult[Rise] = e match {
        case DepApp(DepApp(Slide(), n: Nat), Cst(1)) =>
          Success(
            TypedDSL.oclRotateValues(a)(n)(untyped(write))
              :: e.t)
        case _ => Failure(rotateValues(a, write))
      }
      override def toString = s"rotateValues($a, $write)"
    }
  }
}
