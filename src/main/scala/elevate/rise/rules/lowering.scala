package elevate.rise.rules

import arithexpr.arithmetic.Cst
import elevate.core.strategies.basic._
import elevate.core.strategies.predicate._
import elevate.core.strategies.traversal._
import elevate.core.{Failure, Strategy, Success}
import elevate.macros.RuleMacro.rule
import elevate.rise._
import elevate.rise.rules.traversal._
import elevate.rise.strategies.normalForm.DFNF
import elevate.rise.strategies.predicate._
import elevate.rise.strategies.predicate.isVectorizeablePrimitive.isVectorArray
import rise.core.TypedDSL._
import rise.core._
import rise.core.primitives._
import rise.core.types._
import rise.openMP.TypedDSL.mapPar

object lowering {

  // Straight-forward Lowering

  @rule def mapSeq: Strategy[Rise] = {
    case m@Map() => Success(MapSeq()(m.t) :: m.t)
  }

  @rule def mapStream: Strategy[Rise] = {
    case m@Map() => Success(MapStream()(m.t) :: m.t)
  }

  @rule def iterateStream: Strategy[Rise] = {
    case m@Map() => Success(IterateStream()(m.t) :: m.t)
  }

  @rule def mapSeqUnroll: Strategy[Rise] = {
    case m@Map() => Success(MapSeqUnroll()(m.t) :: m.t)
  }

  @rule def mapGlobal(dim: Int = 0): Strategy[Rise] = {
    case m@Map() => Success(rise.openCL.TypedDSL.mapGlobal(dim) :: m.t)
  }

  @rule def reduceSeq: Strategy[Rise] = {
    case e@Reduce() => Success(TypedDSL.reduceSeq :: e.t)
  }

  // todo shall we allow lowering from an already lowered reduceSeq?
  @rule def reduceSeqUnroll: Strategy[Rise] = {
    case e@ReduceX() => Success(TypedDSL.reduceSeqUnroll :: e.t)
  }

  // Specialized Lowering

  @rule def mapSeqCompute: Strategy[Rise] = {
    case App(Map(), f) if containsComputation(f) && not(isMappingZip)(f) =>
      Success(TypedDSL.mapSeq(f))
  }

  @rule def isMappingZip: Strategy[Rise] = {
    case l@Lambda(_, App(App(Zip(), a), b)) => Success(l)
    case m@Lambda(_, App(App(Map(), f), arg)) => isMappingZip(f)
  }

  // TODO: load identity instead, then change with other rules?
  @rule def circularBuffer(load: Expr): Strategy[Rise] = {
    case e@DepApp(DepApp(Slide(), sz: Nat), Cst(1)) => Success(
      TypedDSL.circularBuffer(sz)(sz)(untyped(load)) :: e.t)
  }

  @rule def rotateValues(write: Expr): Strategy[Rise] = {
    case e@DepApp(DepApp(Slide(), sz: Nat), Cst(1)) => Success(
      TypedDSL.rotateValues(sz)(untyped(write)) :: e.t)
  }

  @rule def containsComputation: Strategy[Rise] = topDown(isComputation)

  // requires type information!
  @rule def isComputation: Strategy[Rise] = e => {
    def isPairOrBasicType(t: Type): Boolean = t match {
      case _: BasicType => true
      case PairType(a, b) => isPairOrBasicType(a) && isPairOrBasicType(b)
      case _ => false
    }

    e match {
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
  @rule def mapSeqUnrollWrite: Strategy[Rise] = e => e.t match {
    case ArrayType(_, _: BasicType) =>
      Success(app(TypedDSL.mapSeqUnroll(fun(x => x)), typed(e)) :: e.t)
    case _ =>
      Failure(mapSeqUnrollWrite)
  }

  @rule def toMemAfterMapSeq: Strategy[Rise] = {
    case a@App(App(MapSeq(), _), _) =>
      Success((typed(a) |> TypedDSL.toMem) :: a.t)
  }

  // Lowerings used in PLDI submission

  // adds copy after every generate
  val materializeGenerate: Strategy[Rise] =
    normalize(
      argument(function(isGenerate)) `;`
        not(isCopy) `;`
        argument(copyAfterGenerate)
    )

  // adds explicit copies for every init value in reductions
  val materializeInitOfReduce: Strategy[Rise] =
    normalize(
      function(function(isReduceX)) `;`
        argument(not(isCopy) `;` insertCopyAfter)
    )

  @rule def insertCopyAfter: Strategy[Rise] = e => {
    def constructCopy(t: Type): TDSL[Rise] = t match {
      case ArrayType(_, dt) => TypedDSL.mapSeq(fun(x => constructCopy(dt) $ x))
      case _: BasicType => fun(x => x)
      case _ => ??? // shouldn't happen?
    }

    Success(constructCopy(e.t) $ e)
  }

  // todo currently only works for mapSeq
  @rule def isCopy: Strategy[Rise] = {
    case c@App(Let(), id) if isId(id) => Success(c)
    case c@App(App(MapSeq(), id), etaInput) if isId(id) => Success(c)
    case App(App(MapSeq(), Lambda(_, f)), etaInput) => isCopy(f)
    case c@App(id, _) if isId(id) => Success(c)
  }

  @rule def isId: Strategy[Rise] = {
    case l@Lambda(x1, x2) if x1 == x2 => Success(l)
  }

  // requires expr to be in LCNF
  val specializeSeq: Strategy[Rise] =
    normalize(lowering.mapSeqCompute <+ lowering.reduceSeq)

  val addRequiredCopies: Strategy[Rise] =
    // `try`(oncetd(copyAfterReduce)) `;` LCNF `;` materializeInitOfReduce
    tryAll(copyAfterReduce) `;` DFNF `;` materializeInitOfReduce

  // todo gotta use a normalform for introducing copies! e.g., if we have two reduce primitives
  val lowerToC: Strategy[Rise] = addRequiredCopies `;` `try`(bottomUp(copyAfterReduceInit)) `;` specializeSeq


  // todo currently only works for mapSeq
  @rule def copyAfterReduce: Strategy[Rise] = e => {
    def constructCopy(t: Type): TDSL[Rise] = t match {
      case _: BasicType => let(fun(x => x))
      case ArrayType(_, _: BasicType) => TypedDSL.mapSeq(fun(x => x))
      case ArrayType(_, a: ArrayType) => TypedDSL.mapSeq(fun(x => constructCopy(a) $ x))
      case _ => ??? // shouldn't happen?
    }

    e match {
      case reduceResult@App(App(App(ReduceX(), _), _), _) =>
        Success(constructCopy(reduceResult.t) $ reduceResult)
      case _ => Failure(copyAfterReduce)
    }
  }

  @rule def copyAfterReduceInit: Strategy[Rise] = e => {
    def constructCopy(t: Type): TDSL[Rise] = t match {
      case _: BasicType => let(fun(x => x))
      case ArrayType(_, _: BasicType) => TypedDSL.mapSeq(fun(x => x))
      case ArrayType(_, a: ArrayType) => TypedDSL.mapSeq(fun(x => constructCopy(a) $ x))
      case x => println(x) ; ??? // shouldn't happen?
    }

    e match {
      case App(a@App(ReduceSeqUnroll(), _), init) =>
        Success(TDSL(a) $ (constructCopy(init.t) $ init))
      case _ => Failure(copyAfterReduce)
    }
  }

  // todo currently only works for mapSeq
  @rule def copyAfterGenerate: Strategy[Rise] = e => {
    def constructCopy(t: Type): TDSL[Rise] = t match {
      case ArrayType(_, dt) => TypedDSL.mapSeq(fun(x => constructCopy(dt) $ x))
      case _: BasicType => fun(x => x)
      case _ => ??? // shouldn't happen?
    }

    e match {
      case a@App(Generate(), _) => Success(constructCopy(a.t) $ a)
      case _ => Failure(copyAfterGenerate)
    }
  }

  @rule def vectorize(n: Nat): Strategy[Rise] = e => {
    def vectorizeArrayBasedOnType(t: Type): TDSL[Rise] = {
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

    e match {
      case a@App(App(Map(), f), input) if
      isComputation(f) && !isVectorArray(a.t) =>

        val newF = untyped(f)
        Success(
          (asScalar o map(newF)) $ (vectorizeArrayBasedOnType(input.t) $ input)
        )
      case _ => Failure(vectorize(n))
    }
  }

  @rule def untype: Strategy[Rise] = p => Success(p.setType(TypePlaceholder))

  @rule def parallel: Strategy[Rise] = {
    case App(Map(), f) if containsComputation(f) => Success(mapPar(f))
  }

  @rule def unroll: Strategy[Rise] = {
    case ReduceSeq() => Success(TypedDSL.reduceSeqUnroll)
  }

  object ocl {
    import rise.core.types.AddressSpace
    import rise.openCL.TypedDSL
    import rise.openCL.primitives._

    @rule def reduceSeqUnroll(a: AddressSpace): Strategy[Rise] = {
      case e@ReduceX() => Success(TypedDSL.oclReduceSeqUnroll(a) :: e.t)
    }

    @rule def circularBuffer(a: AddressSpace): Strategy[Rise] = {
      case e@DepApp(DepApp(Slide(), n: Nat), Cst(1)) =>
        Success(
          TypedDSL.oclCircularBuffer(a)(n)(n)(fun(x => x))
            :: e.t)
    }

    @rule def circularBufferLoadFusion: Strategy[Rise] = {
      case e@App(App(
        cb @ DepApp(DepApp(DepApp(OclCircularBuffer(), _), _), _),
        load), App(App(Map(), f), in)
      ) =>
        Success(untyped(cb)(typed(f) >> load, in) :: e.t)
    }

    @rule def rotateValues(a: AddressSpace, write: Expr): Strategy[Rise] = {
      case e@DepApp(DepApp(Slide(), n: Nat), Cst(1)) =>
        Success(
          TypedDSL.oclRotateValues(a)(n)(untyped(write))
            :: e.t)
    }
  }
}
