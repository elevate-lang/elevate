package elevate.rise.rules

import elevate.core._
import elevate.core.strategies.predicate._
import elevate.rise.strategies.predicate._
import elevate.rise.rules.traversal._
import elevate.rise._
import rise.core._
import rise.core.TypedDSL._
import rise.core.primitives._
import rise.core.types._
import arithexpr.arithmetic.Cst

//noinspection MutatorLikeMethodIsParameterless
object algorithmic {

  // - Notation -
  // x >> y: piping operator, x then y
  // *f: map(f)
  // T: transpose
  // S: slide/split
  // J: join

  // divide & conquer

  def  splitJoin(n: Nat): Strategy[Rise] = `*f -> S >> **f >> J`(n: Nat)
  case class `*f -> S >> **f >> J`(n: Nat) extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = e match {
      case App(Map(), f) => Success((split(n) >> map(map(f)) >> join) :: e.t)
      case _             => Failure(splitJoin(n))
    }
    override def toString = s"splitJoin($n)"
  }

  // fusion / fission

  def mapFusion: Strategy[Rise] = `*g >> *f -> *(g >> f)`
  case object `*g >> *f -> *(g >> f)` extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = e match {
      case App(App(Map(), f), App(App(Map(), g), arg)) => Success(map(typed(g) >> f)(arg) :: e.t)
      case _                                           => Failure(mapFusion)
    }
    override def toString = s"mapFusion"
  }

  // fission of the last function to be applied inside a map
  def mapLastFission: Strategy[Rise] = `*(g >> .. >> f) -> *(g >> ..) >> *f`
  case object `*(g >> .. >> f) -> *(g >> ..) >> *f` extends Strategy[Rise] {
    // this is an example where we don't want to fission if gx == Identifier:
    // (map λe4. (((((zip: (K.float -> (K.float -> K.(float, float)))) (e3: K.float)): (K.float -> K.(float, float))) (e4: K.float)): K.(float, float)))
    // gx == (e4: K.float)
    // in this case we would return some form of map(id):
    // ((map λe4. (e4: K.float)) e743))
    def apply(e: Rise): RewriteResult[Rise] = e match {
      case App(Map(), Lambda(x, App(f, gx))) if !contains[Rise](x).apply(f) && !isIdentifier(gx) =>
        Success((app(map, lambda(untyped(x), gx)) >> map(f)) :: e.t)
      case _ => Failure(mapLastFission)
    }
    override def toString = s"mapLastFission"
  }

  // identities

  def idAfter: Strategy[Rise] = ` -> id`
  case object ` -> id` extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = Success((typed(e) |> id) :: e.t)
    override def toString = "idAfter"
  }

  def liftId: Strategy[Rise] = `id -> *id`
  case object `id -> *id` extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = e match {
      case App(Id(), arg) => Success(app(map(id), arg) :: e.t)
      case _              => Failure(liftId)
    }
    override def toString = "liftId"
  }

  def createTransposePair: Strategy[Rise] = `id -> T >> T`
  case object `id -> T >> T` extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = e match {
      case App(Id(), arg) => Success(app(transpose >> transpose, arg) :: e.t)
      case _              => Failure(createTransposePair)
    }
    override def toString = "createTransposePair"
  }

  def `_-> T >> T`: Strategy[Rise] = idAfter `;` createTransposePair

  def removeTransposePair: Strategy[Rise] = `T >> T -> `
  case object `T >> T -> ` extends Strategy[Rise]  {
    def apply(e: Rise): RewriteResult[Rise] = e match {
      case App(Transpose(), App(Transpose(), x)) => Success(x :: e.t)
      case _                                     => Failure(removeTransposePair)
    }
    override def toString = "createTransposePair"
  }

  // overlapped tiling

  // constraint: n - m = u - v
  // v = u + m - n
  def slideOverlap(u: Nat): Strategy[Rise] =
    `slide(n, m) -> slide(u, v) >> *(slide(n, m)) >> J`(u)
  def `slide(n, m) -> slide(u, v) >> *(slide(n, m)) >> J`(
   u: Nat
  ): Strategy[Rise] = {
    case expr @ DepApp(DepApp(Slide(), n: Nat), m: Nat) =>
      val v = u + m - n
      Success((slide(u)(v) >> map(slide(n)(m)) >> join) :: expr.t)
    case _ =>
      Failure(slideOverlap(u))
  }

  // slide widening

  def dropInSlide: Strategy[Rise] =
    `slide n 1 >> drop l -> slide (n+l) 1 >> map(drop l)`
  def `slide n 1 >> drop l -> slide (n+l) 1 >> map(drop l)`: Strategy[Rise] = {
    case expr @ App(DepApp(Drop(), l: Nat),
      App(DepApp(DepApp(Slide(), n: Nat), Cst(1)), in)
    ) =>
      Success(app(map(drop(l)), app(slide(n+l)(1), typed(in))) :: expr.t)
    case _ =>
      Failure(dropInSlide)
  }

  def takeInSlide: Strategy[Rise] =
    `slide n 1 >> take (N - r) -> slide (n+r) 1 >> map(take (n - r))`
  def `slide n 1 >> take (N - r) -> slide (n+r) 1 >> map(take (n - r))`: Strategy[Rise] = {
    case expr @ App(t @ DepApp(Take(), rem: Nat),
    App(DepApp(DepApp(Slide(), n: Nat), Cst(1)), in)
    ) =>
      t.t match {
        case FunType(ArrayType(size, _), _) =>
          val r = size - rem
          Success(app(map(take(n)), app(slide(n+r)(1), typed(in))) :: expr.t)
        case _ => throw new Exception("this should not happen")
      }
    case _ =>
      Failure(dropInSlide)
  }

  // makeArray(n)(map f1 e)..(map fn e)
  // -> e |> map(fun(x => makeArray(n)(f1 x)..(fn x))) |> transpose
  case object mapOutsideMakeArray extends Strategy[Rise] {
    def matchExpectedMakeArray(mka: Rise): Option[Rise] = mka match {
      case App(MakeArray(_), App(App(Map(), _), e)) => Some(e)
      case App(f, App(App(Map(), _), e2)) =>
        matchExpectedMakeArray(f).flatMap(e =>
          if (e == e2) { Some(e) } else { None })
      case _ => None
    }

    def transformMakeArray(mka: Rise, x: TDSL[Rise]): TDSL[Rise] = mka match {
      case MakeArray(n) => array(n)
      case App(mka, App(App(Map(), f), _)) =>
        app(transformMakeArray(mka, x), app(f, x))
      case _ => throw new Exception("this should not happen")
    }

    def apply(expr: Rise): RewriteResult[Rise] =
      matchExpectedMakeArray(expr) match {
        case Some(e) => Success(
          app(transpose, map(fun(x => transformMakeArray(expr, x)))(e))
            :: expr.t)
        case None => Failure(mapOutsideMakeArray)
      }
  }

  // TODO: should not be in this file?
  // broadly speaking, f(x) -> x |> fun(y => f(y))
  case class subexpressionElimination(x: Rise) extends Strategy[Rise] {
    import elevate.core.strategies.traversal._
    def apply(e: Rise): RewriteResult[Rise] = {
      var typedX: Rise = null // Hack to get the typed version of X
      oncetd(isEqualTo(x) `;` { xt =>
        typedX = xt
        Success(xt)
      }).apply(e).mapSuccess(_ => {
        app(fun(typedX.t)(y =>
          substitute.exprInExpr(y, `for` = typedX, e)
        ), typedX) :: e.t
      })
    }
  }
}
