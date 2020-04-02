package elevate.rise.rules

import arithexpr.arithmetic.Cst
import elevate.core._
import elevate.rise._
import rise.core._
import rise.core.types._
import rise.core.primitives._
import rise.core.TypedDSL._

object vectorize {
  // FIXME: sometimes assuming loads or stores will be aligned

  // _ -> asVector >> asScalar
  def after(n: Nat): Strategy[Rise] = e => e.t match {
    // FIXME: m + n hack
    case ArrayType(m, _: ScalarType) if (m + n) % n == (0: Nat) =>
      Success(asScalar(asVector(n)(e)) :: e.t)
    case _ => Failure(after(n))
  }

  // _ -> padEmpty >> asVector >> asScalar >> take
  def roundUpAfter(n: Nat): Strategy[Rise] = e => e.t match {
    case ArrayType(m, _: ScalarType) =>
      val roundUp = padEmpty(n - ((m + n) % n)) // FIXME: m + n hack
      Success(take(m)(asScalar(asVector(n)(roundUp(e)))) :: e.t)
    case _ => Failure(after(n))
  }

  // _ -> asVectorAligned >> asScalar
  def alignedAfter(n: Nat): Strategy[Rise] = e => e.t match {
    // FIXME: m + n hack
    case ArrayType(m, _: ScalarType) if (m + n) % n == (0: Nat) =>
      Success(asScalar(asVectorAligned(n)(e)) :: e.t)
    case _ => Failure(alignedAfter(n))
  }

  def isAsVector: Rise => Boolean = {
    case DepApp(AsVector(), _: Nat) => true
    case DepApp(AsVectorAligned(), _: Nat) => true
    case _ => false
  }

  // asScalar >> asVector -> _
  def asScalarAsVectorId: Strategy[Rise] = {
    case e @ App(v, App(AsScalar(), in)) if isAsVector(v) && e.t == in.t =>
      Success(in)
    case _ => Failure(asScalarAsVectorId)
  }

  // FIXME: assuming every scalar function vectorizes like this
  private def vectorizeScalarFun(f: Expr, vEnv: Set[Identifier])
  : TDSL[Expr] = f match {
    case i: Identifier if vEnv(i) => untyped(i)
    case Lambda(x, b) => lambda(untyped(x), vectorizeScalarFun(b, vEnv + x))
    case App(f, e) => vectorizeScalarFun(f, vEnv)(vectorizeScalarFun(e, vEnv))
    case p: Primitive => untyped(p)
    case s if (s.t match {
      case _: ScalarType => true
      case _ => false
    }) => vectorFromScalar(s)
    case _ => throw new Exception(s"did not expect $f")
  }

  private def isScalarFun: Type => Boolean = {
    case FunType(i, o) => isScalarTuple(i) &&
      (isScalarTuple(o) || isScalarFun(o))
    case _ => false
  }

  private def isScalarTuple: Type => Boolean = {
    case _: ScalarType => true
    case PairType(a, b) => isScalarTuple(a) && isScalarTuple(b)
    case _ => false
  }

  def makeAsVector(asV: Rise): Type => TDSL[Rise] = {
    case ArrayType(_, _: ScalarType) => untyped(asV)
    case ArrayType(n, PairType(a, b)) =>
      unzip >> fun(p => zip(
        makeAsVector(asV)(ArrayType(n, a))(fst(p)),
        makeAsVector(asV)(ArrayType(n, b))(snd(p))))
    case t => throw new Exception(s"did not expect $t")
  }

  // map (reduce f init) >> asVector -> asVector >> map (reduce f init)
  def beforeMapReduce: Strategy[Rise] = {
    case e @ App(v, App(App(Map(), App(App(Reduce(), f), init)), in))
    if isAsVector(v) && isScalarFun(f.t) =>
      // TODO: generalize?
      val inV = typed(in) |> transpose |> map(untyped(v)) |> transpose
      val fV = vectorizeScalarFun(f, Set())
      Success(map(reduce(fV, vectorFromScalar(init)), inV) :: e.t)
    case _ => Failure(beforeMapReduce)
  }

  // TODO: express as a combination of beforeMapReduce, beforeMap, and others.
  // a |> map (zip b) |> map (reduce f init) |> asVector
  // -> a |> transpose |> map(asVector) |> transpose |> ..
  def beforeMapDot: Strategy[Rise] = {
    case e @ App(v, App(App(Map(), App(r @ App(ReduceMaybeSeq(), f), init)),
      App(App(Map(), App(Zip(), b)), a)
    )) if isAsVector(v) && isScalarFun(f.t) =>
      val aV = typed(a) |> transpose |> map(untyped(v)) |> transpose
      val bV = map(vectorFromScalar, b)
      val rV = vectorizeScalarFun(r, Set())
      Success(map(zip(bV) >> rV(vectorFromScalar(init)), aV) :: e.t)
    case _ => Failure(beforeMapReduce)
  }

  // map f >> asVector -> asVector >> map f
  def beforeMap: Strategy[Rise] = {
    case e @ App(v, App(App(Map(), f), in))
    if isAsVector(v) && isScalarFun(f.t) =>
      val inV = makeAsVector(v)(in.t)(in)
      val fV = vectorizeScalarFun(f, Set())
      Success(map(fV, inV) :: e.t)
    case _ => Failure(beforeMap)
  }

  // pair (asScalar a) (asScalar b)
  // -> pair a b >> mapFst asScalar >> mapSnd asScalar
  // TODO: can get any function out, see takeOutsidePair
  def asScalarOutsidePair: Strategy[Rise] = {
    case e @ App(App(Pair(), App(AsScalar(), a)), App(AsScalar(), b)) =>
      Success((pair(a, b) |> mapFst(asScalar) |> mapSnd(asScalar)) :: e.t)
    case _ => Failure(asScalarOutsidePair)
  }

  // zip (asScalar a) (asScalar b)
  // -> pair a b >> mapFst asScalar >> mapSnd asScalar
  def asScalarOutsideZip: Strategy[Rise] = {
    case e @ App(App(Pair(), App(AsScalar(), a)), App(AsScalar(), b)) =>
      Success((pair(a, b) |> mapFst(asScalar) |> mapSnd(asScalar)) :: e.t)
    case _ => Failure(asScalarOutsidePair)
  }

  // TODO: express as a combination of smaller rules
  def alignSlide: Strategy[Rise] = {
    case e @ App(Transpose(),
      App(App(Map(), DepApp(AsVector(), Cst(v))),
        App(Join(), App(App(Map(), Transpose()),
          App(App(Map(), DepApp(PadEmpty(), Cst(p))),
            App(App(Map(), DepApp(DepApp(Slide(), Cst(3)), Cst(1))),
              in
            )
          )
        ))
      )
    ) if p <= v =>
      val inW = in.t.asInstanceOf[ArrayType].elemType
        .asInstanceOf[ArrayType].size
      val pV = if (((inW + v) % v).eval == 0) { // TODO: generalize
        Cst(v)
      } else {
        Cst(v + v) - ((inW + v) % v)
      }
      val r = typed(in) |>
        map(padEmpty(pV) >> asVectorAligned(v) >> slide(2)(1)) >>
        transpose >>
        map(
          map(asScalar >> take(v+2) >> slide(v)(1) >> join >> asVector(v)) >>
          join
        )
      Success(r :: e.t)
    case _ => Failure(alignSlide)
  }

  def makeShuffle(s: Rise): Type => TDSL[Rise] = {
    case ArrayType(_, _: VectorType) => untyped(s)
    case ArrayType(n, PairType(a, b)) =>
      unzip >> fun(p => zip(
        makeShuffle(s)(ArrayType(n, a))(fst(p)),
        makeShuffle(s)(ArrayType(n, b))(snd(p))))
    case t => throw new Exception(s"did not expect $t")
  }

  // TODO: express as a combination of smaller rules
  // FIXME: function f needs to be element-wise (a hidden mapVec)
  def mapAfterShuffle: Strategy[Rise] = {
    case e @ App(DepApp(AsVector(), v: Nat),
      App(Join(), App(DepApp(DepApp(Slide(), v2: Nat), Cst(1)),
        App(DepApp(Take(), t: Nat), App(AsScalar(),
          App(App(Map(), f), in)
        ))
      ))
    ) if v == v2 =>
      val shuffle = makeShuffle(
        asScalar >> take(t) >>
        slide(v)(1) >> join >> asVector(v)
      )(in.t)
      Success((typed(in) |> shuffle |> map(f)) :: e.t)
    case _ => Failure(mapAfterShuffle)
  }
}
