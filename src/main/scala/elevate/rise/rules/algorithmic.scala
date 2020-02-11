package elevate.rise.rules

import elevate.core._
import elevate.core.strategies.predicate._
import elevate.core.strategies.traversal.tryAll
import elevate.rise.strategies.predicate._
import elevate.rise.rules.traversal._
import elevate.rise._
import elevate.rise.strategies.normalForm.LCNF
import rise.core._
import rise.core.TypedDSL._
import rise.core.primitives._
import rise.core.types._

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

  // result needs to be a reduceSeq always?
  case object fuseReduceMap extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = e match {
      case App(
      //reduce
      App(App(ReduceX(), op), init),
      //map
      App(App(Map(), f), mapArg)
      ) =>
        val red = op.t match {
          case FunType(a, FunType(b, c)) if a == b && b == c => reduce
          case FunType(_, FunType(_, _)) => reduceSeq
          case _ => ???
        }
        Success(
          (red(fun((acc, y) =>
            typed(op)(acc)(typed(f)(y))))(init) $ mapArg) :: e.t)

      case _ => Failure(fuseReduceMap)
    }
  }

  case object fissionReduceMap extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = e match {
      // todo: think about reduce case
      case App(App(App(ReduceSeq(),
            Lambda(acc, Lambda(y,
              App(App(op, acc2), a@App(f, y2))
            ))), init), arg) if
      acc == acc2 && contains[Rise](y).apply(y2) =>
        Success((reduce(op)(init) o map(lambda(TDSL[Identifier](y), typed(a)))) $ arg)
      case _ => Failure(fissionReduceMap)
    }
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

  // slideSeq fusion
  import rise.OpenCL.primitives._
  import rise.OpenCL.TypedDSL._

  def slideSeqFusion: Strategy[Rise] = `slideSeq(f) >> map(g) -> slideSeq(f >> g)`
  def `slideSeq(f) >> map(g) -> slideSeq(f >> g)`: Strategy[Rise] = {
    case expr@App(App(Map(), g), App(App(App(DepApp(DepApp(SlideSeq(rot), sz: Nat), sp: Nat), wr), f), e)) =>
      Success(slideSeq(rot)(sz)(sp)(wr)(typed(f) >> g)(e) :: expr.t)
    case expr@App(App(Map(), g), App(App(App(DepApp(DepApp(DepApp(OclSlideSeq(rot), a: AddressSpace), sz: Nat), sp: Nat), wr), f), e)) =>
      Success(oclSlideSeq(rot)(a)(sz)(sp)(wr)(typed(f) >> g)(e) :: expr.t)
    case _ => Failure(slideSeqFusion)
  }

  // the inner strategies shouldn't be accessible from the outside
  // because they might change the semantics of a program
  case object freshLambdaIdentifier extends Strategy[Rise] {
    case object freshIdentifier extends Strategy[Rise] {
      def apply(e: Rise): RewriteResult[Rise] = e match {
        case Identifier(name) :: t => Success(Identifier(freshName("fresh_"+ name))(t))
        case _ => Failure(freshIdentifier)
      }
    }

    case class replaceIdentifier(curr: Identifier, newId: Identifier) extends Strategy[Rise] {
      def apply(e: Rise): RewriteResult[Rise] = e match {
        case x:Identifier if curr == x => Success(newId)
        case _ => Failure(replaceIdentifier(curr, newId))
      }
    }

    def apply(e: Rise): RewriteResult[Rise] = e match {
      case Lambda(x,e) :: t if contains[Rise](x).apply(e) => {
        val newX = freshIdentifier(x).get.asInstanceOf[Identifier]
        val newE = tryAll(replaceIdentifier(x, newX)).apply(e).get
        Success(Lambda(newX, newE)(t))
      }
      case _ => Failure(freshLambdaIdentifier)
    }
  }


  // requires type information!
  case class blockedReduce(n: Nat) extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = e match {
      case App(App(App(Reduce(), op :: FunType(yT, FunType(initT, outT))), init), arg) if yT == outT =>
        // avoid having two lambdas using the same identifiers
        val freshOp = tryAll(freshLambdaIdentifier).apply(op).get
        Success(
          LCNF((reduceSeq(fun((acc, y) =>
            typed(op)(acc, reduce(freshOp)(init)(y))))(init) o split(n)) $ arg))

      case _ => Failure(blockedReduce(n))
    }
  }

}
