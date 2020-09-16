package elevate.rise.rules

import elevate.core.strategies.Traversable
import elevate.core.strategies.predicate._
import elevate.core.{Failure, RewriteResult, Strategy, Success}
import elevate.rise._
import rise.core._
import rise.core.types._
import rise.core.primitives._
import rise.core.TypedDSL._
import rise.core.TypeLevelDSL._
import rise.core.types.{ArrayType, DataType, FunType, IndexType, Nat, PairType}

// Describing possible movements between pairs of rise primitives
// (potentially nested in maps)

// todo: remove inspection prevention

//noinspection ScalaStyle
// todo: should all rules expect LCNF-normalized expressions as input?
object movement {

  // - Notation -
  // x >> y: piping operator, x then y
  // *f: map(f)
  // T: transpose
  // S: slide/split
  // J: join

  // transpose

  def mapMapFBeforeTranspose(implicit ev: Traversable[Rise]): Strategy[Rise] = `**f >> T -> T >> **f`()
  case class `**f >> T -> T >> **f`()(implicit ev: Traversable[Rise]) extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = e match {
      case App(
        Transpose(),
        App(App(Map(), App(Map(), f)), y)) =>
          Success((typed(y) |> transpose |> map(map(f))) :: e.t)
      // LCNF
      case App(Transpose(),
      App(
      App(Map(), lamA @ Lambda(_, App(
                App(Map(), lamB @ Lambda(_, App(
      f, _))), _))),
      arg
      )
      ) if etaReduction()(ev)(lamA) && etaReduction()(ev)(lamB) =>
        // Success((typed(arg) |> transpose |> map(map(f))) :: e.t)
        Success((typed(arg) |> transpose |> map(fun(a => map(fun(b => typed(f)(b)))(a)))) :: e.t)
      case _ => Failure(mapMapFBeforeTranspose)
    }
    override def toString = "mapMapFBeforeTranspose"
  }

  def transposeBeforeMapMapF: Strategy[Rise] = `T >> **f -> **f >> T`
  case object `T >> **f -> **f >> T` extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = e match {
      case App(
      App(Map() , App(Map(), f)),
      App(Transpose() , y)) =>
        Success((typed(y) |> map(map(f)) |> transpose) :: e.t)
      case _ => Failure(transposeBeforeMapMapF)
    }
    override def toString = "transposeBeforeMapMapF"
  }

  // split/slide

  private def isSplitOrSlide(s: Expr): Boolean = s match {
    case DepApp(DepApp(Slide(), _: Nat), _: Nat) => true
    case DepApp(Split(), _: Nat)                 => true
    case _                                       => false
  }

  def slideBeforeMapMapF: Strategy[Rise] = `S >> **f -> *f >> S`
  case object `S >> **f -> *f >> S` extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = e match {
      case App(
      App(Map(), App(Map(), f)),
      App(s, y)) if isSplitOrSlide(s) =>
        Success((typed(y) |> map(f) |> untyped(s)) :: e.t)
      case _ => Failure(slideBeforeMapMapF)
    }
    override def toString = "slideBeforeMapMapF"
  }

  def slideBeforeMap: Strategy[Rise] = `*f >> S -> S >> **f`
  case object `*f >> S -> S >> **f` extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = e match {
      case App(s @ DepApp(DepApp(Slide(), _: Nat), _: Nat),
        App(App(Map(), f), y)
      ) =>
        Success((typed(y) |> untyped(s) |> map(map(f))) :: e.t)
      case _ => Failure(slideBeforeMap)
    }
    override def toString = "slideBeforeMap"
  }

//  def mapFBeforeSlide: Strategy[Rise] = `*f >> S -> S >> **f`
//  case object `*f >> S -> S >> **f` extends Strategy[Rise] {
//    def apply(e: Rise): RewriteResult[Rise] = e match {
//      case App(
//      s,
//      App(App(Map(), f), y)) if isSplitOrSlide(s) =>
//        Success((typed(y) |> untyped(s) |> map(map(f))) :: e.t)
//      case _ => Failure(mapFBeforeSlide)
//    }
//    override def toString = "mapFBeforeSlide"
//  }

  // *f >> S -> S >> **f
  case object splitBeforeMap extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = e match {
      case App(s @ DepApp(Split(), _: Nat),
        App(App(Map(), f), y)
      ) =>
        Success((typed(y) |> untyped(s) |> map(map(f))) :: e.t)
      case _ => Failure(splitBeforeMap)
    }
    override def toString = "splitBeforeMap"
  }

  // join

  def joinBeforeMapF: Strategy[Rise] = `J >> *f -> **f >> J`
  case object `J >> *f -> **f >> J` extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = e match {
      case App(
      App(Map(), f),
      App(Join(), y)
      ) =>
        Success((typed(y) |> map(map(f)) >> join) :: e.t)
      case _ => Failure(joinBeforeMapF)
    }
    override def toString = "joinBeforeMapF"
  }

  def mapMapFBeforeJoin: Strategy[Rise] = `**f >> J -> J >> *f`
  case object `**f >> J -> J >> *f` extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = e match {
      case App(
      Join(),
      App(App(Map(), App(Map(), f)), y)
      ) =>
        Success((typed(y) |> join |> map(f)) :: e.t)
      case _ => Failure(mapMapFBeforeJoin)
    }
    override def toString = "mapMapFBeforeJoin"
  }

  // drop and take

  def dropBeforeMap: Strategy[Rise] = `*f >> drop n -> drop n >> *f`
  def `*f >> drop n -> drop n >> *f`: Strategy[Rise] = {
    case expr @ App(DepApp(Drop(), n: Nat), App(App(Map(), f), in)) =>
      Success(app(map(f), app(drop(n), typed(in))) :: expr.t)
    case _ =>
      Failure(dropBeforeMap)
  }

  def takeBeforeMap: Strategy[Rise] = `*f >> take n -> take n >> *f`
  def `*f >> take n -> take n >> *f`: Strategy[Rise] = {
    case expr @ App(DepApp(Take(), n: Nat), App(App(Map(), f), in)) =>
      Success(app(map(f), app(take(n), typed(in))) :: expr.t)
    case _ =>
      Failure(takeBeforeMap)
  }

  // take n >> *f -> *f >> take n
  def takeAfterMap: Strategy[Rise] = {
    case e @ App(App(Map(), f), App(DepApp(Take(), n: Nat), in)) =>
      Success(take(n)(map(f, in)) :: e.t)
    case _ => Failure(takeAfterMap)
  }

  def takeInZip: Strategy[Rise] = `take n (zip a b) -> zip (take n a) (take n b)`
  def `take n (zip a b) -> zip (take n a) (take n b)`: Strategy[Rise] = {
    case expr @ App(DepApp(Take(), n), App(App(Zip(), a), b)) =>
      Success(zip(depApp(take, n)(a), depApp(take, n)(b)) :: expr.t)
    case _ => Failure(takeInZip)
  }

  // zip (take n a) (take n b) -> take n (zip a b)
  def takeOutisdeZip: Strategy[Rise] = {
    case e @ App(App(Zip(),
      App(DepApp(Take(), n1: Nat), a)), App(DepApp(Take(), n2: Nat), b)
    ) if n1 == n2 =>
      Success(take(n1)(zip(a, b)) :: e.t)
    case _ => Failure(takeOutisdeZip)
  }

  // pair (take n a) (take m b) -> pair a b >> mapFst take n >> mapSnd take m
  // TODO: can get any function out, see asScalarOutsidePair
  def takeOutsidePair: Strategy[Rise] = {
    case e @ App(App(Pair(),
      App(DepApp(Take(), n: Nat), a)), App(DepApp(Take(), m: Nat), b)
    ) =>
      Success((pair(a, b) |> mapFst(take(n)) |> mapSnd(take(m))) :: e.t)
    case _ => Failure(takeOutsidePair)
  }

  def dropInZip: Strategy[Rise] = `drop n (zip a b) -> zip (drop n a) (drop n b)`
  def `drop n (zip a b) -> zip (drop n a) (drop n b)`: Strategy[Rise] = {
    case expr @ App(DepApp(Drop(), n), App(App(Zip(), a), b)) =>
      Success(zip(depApp(drop, n)(a), depApp(drop, n)(b)) :: expr.t)
    case _ => Failure(dropInZip)
  }

  def takeInSelect: Strategy[Rise] = `take n (select t a b) -> select t (take n a) (take n b)`
  def `take n (select t a b) -> select t (take n a) (take n b)`: Strategy[Rise] = {
    case expr @ App(DepApp(Take(), n), App(App(App(Select(), t), a), b)) =>
      Success(select(t, depApp(take, n)(a), depApp(take, n)(b)) :: expr.t)
    case _ => Failure(takeInSelect)
  }

  def dropInSelect: Strategy[Rise] = `drop n (select t a b) -> select t (drop n a) (drop n b)`
  def `drop n (select t a b) -> select t (drop n a) (drop n b)`: Strategy[Rise] = {
    case expr @ App(DepApp(Drop(), n), App(App(App(Select(), t), a), b)) =>
      Success(select(t, depApp(drop, n)(a), depApp(drop, n)(b)) :: expr.t)
    case _ => Failure(dropInSelect)
  }

  def dropBeforeTake: Strategy[Rise] = `take (n+m) >> drop m -> drop m >> take n`
  def `take (n+m) >> drop m -> drop m >> take n`: Strategy[Rise] = {
    case expr @ App(DepApp(Drop(), m: Nat), App(DepApp(Take(), nm: Nat), in)) =>
      Success(app(take(nm - m), app(drop(m), typed(in))) :: expr.t)
    case _ => Failure(dropBeforeTake)
  }

  def takeBeforeDrop: Strategy[Rise] = `drop m >> take n -> take (n+m) >> drop m`
  def `drop m >> take n -> take (n+m) >> drop m`: Strategy[Rise] = {
    case expr @ App(DepApp(Take(), n: Nat), App(DepApp(Drop(), m: Nat), in)) =>
      Success(app(drop(m), app(take(n+m), typed(in))) :: expr.t)
    case _ => Failure(takeBeforeDrop)
  }

  def takeBeforeSlide: Strategy[Rise] = `slide n m >> take t -> take (m * (t - 1) + n) >> slide n m`
  def `slide n m >> take t -> take (m * (t - 1) + n) >> slide n m`: Strategy[Rise] = {
    case expr @ App(DepApp(Take(), t: Nat), App(DepApp(DepApp(Slide(), n: Nat), m: Nat), in)) =>
      Success(app(slide(n)(m), take(m * (t - 1) + n)(in)) :: expr.t)
    case _ => Failure(takeBeforeSlide)
  }

  def dropBeforeSlide: Strategy[Rise] = `slide n m >> drop d -> drop (d * m) >> slide n m`
  def `slide n m >> drop d -> drop (d * m) >> slide n m`: Strategy[Rise] = {
    case expr @ App(DepApp(Drop(), d: Nat), App(DepApp(DepApp(Slide(), n: Nat), m: Nat), in)) =>
      Success(app(slide(n)(m), drop(d * m)(in)) :: expr.t)
    case _ => Failure(dropBeforeSlide)
  }

  // slide n m >> padEmpty p -> padEmpty (p * m) >> slide n m
  def padEmptyBeforeSlide: Strategy[Rise] = {
    case e @ App(DepApp(PadEmpty(), p: Nat),
      App(DepApp(DepApp(Slide(), n: Nat), m: Nat), in)
    ) =>
      Success(slide(n)(m)(padEmpty(p * m)(in)) :: e.t)
    case _ => Failure(padEmptyBeforeSlide)
  }

  // map f >> padEmpty n -> padEmpty n >> map f
  def padEmptyBeforeMap: Strategy[Rise] = {
    case e @ App(DepApp(PadEmpty(), n: Nat), App(App(Map(), f), in)) =>
      Success(map(f, padEmpty(n)(in)) :: e.t)
    case _ => Failure(padEmptyBeforeMap)
  }

  // transpose >> padEmpty n -> map (padEmpty n) >> transpose
  def padEmptyBeforeTranspose: Strategy[Rise] = {
    case e @ App(DepApp(PadEmpty(), n: Nat), App(Transpose(), in)) =>
      Success(transpose(map(padEmpty(n), in)) :: e.t)
    case _ => Failure(padEmptyBeforeTranspose)
  }

  // padEmpty n (zip a b) -> zip (padEmpty n a) (padEmpty n b)
  def padEmptyInsideZip: Strategy[Rise] = {
    case e @ App(DepApp(PadEmpty(), n: Nat), App(App(Zip(), a), b)) =>
      Success(zip(padEmpty(n)(a), padEmpty(n)(b)) :: e.t)
    case _ => Failure(padEmptyInsideZip)
  }

  // FIXME: this is very specific
  // zip (fst e) (snd e) |> padEmpty n ->
  // (mapFst padEmpty n) (mapSnd padEmpty n) |> fun(p => zip (fst p) (snd(p))
  def padEmptyBeforeZip: Strategy[Rise] = {
    case e @ App(DepApp(PadEmpty(), n: Nat),
      App(App(Zip(), App(Fst(), e1)), App(Snd(), e2)))
    if e1 == e2 =>
      Success((typed(e1) |>
        mapFst(padEmpty(n)) |> mapSnd(padEmpty(n)) |>
        fun(p => zip(fst(p), snd(p)))) :: e.t)
    case _ => Failure(padEmptyBeforeZip)
  }

  // special-cases
  // slide + transpose

  def transposeBeforeSlide: Strategy[Rise] = `T >> S -> *S >> T >> *T`
  case object `T >> S -> *S >> T >> *T` extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = e match {
      case App(
      s,
      App(Transpose(), y)
      ) if isSplitOrSlide(s) =>
        Success((typed(y) |> map(untyped(s)) |> transpose >> map(transpose)) :: e.t)
      case _ => Failure(transposeBeforeSlide)
    }
    override def toString = "transposeBeforeSlide"
  }

  def transposeBeforeMapSlide: Strategy[Rise] = `T >> *S -> S >> *T >> T`
  case object `T >> *S -> S >> *T >> T` extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = e match {
      case App(
      App(Map(), s),
      App(Transpose(), y)
      ) if isSplitOrSlide(s) =>
        Success((typed(y) |> untyped(s) |> map(transpose) |> transpose) :: e.t)
      case _ => Failure(transposeBeforeMapSlide)
    }
    override def toString = "transposeBeforeMapSlide"
  }

  def mapSlideBeforeTranspose: Strategy[Rise] = `*S >> T -> T >> S >> *T`
  case object `*S >> T -> T >> S >> *T` extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = e match {
      case App(
      Transpose(),
      App(App(Map(), s), y)
      ) if isSplitOrSlide(s) =>
        Success((typed(y) |> transpose >> untyped(s) >> map(transpose)) :: e.t)
      case _ => Failure(mapSlideBeforeTranspose)
    }
    override def toString = "mapSlideBeforeTranspose"
  }

  // transpose + join

  def joinBeforeTranspose: Strategy[Rise] = `J >> T -> *T >> T >> *J`
  case object `J >> T -> *T >> T >> *J` extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = e match {
      case App(
      Transpose(),
      App(Join(), y)
      ) =>
        Success((typed(y) |> map(transpose) |> transpose |> map(join)) :: e.t)
      case _ => Failure(joinBeforeTranspose)
    }
    override def toString = "joinBeforeTranspose"
  }

  def transposeBeforeMapJoin: Strategy[Rise] = `T >> *J -> *T >> J >> T`
  case object `T >> *J -> *T >> J >> T` extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = e match {
      case App(
      App(Map(), Join()),
      App(Transpose(), y)
      ) =>
        Success((typed(y) |> map(transpose) |> join |> transpose) :: e.t)
      case _ => Failure(transposeBeforeMapJoin)
    }
    override def toString = "transposeBeforeMapJoin"
  }

  def mapTransposeBeforeJoin: Strategy[Rise] = `*T >> J -> T >> *J >> T`
  case object `*T >> J -> T >> *J >> T` extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = e match {
      case App(
      Join(),
      App(App(Map(), Transpose()), y)
      ) =>
        Success((typed(y) |> transpose |> map(join) |> transpose) :: e.t)
      case _ => Failure(mapTransposeBeforeJoin)
    }
    override def toString = "mapTransposeBeforeJoin"
  }

  def mapJoinBeforeTranspose: Strategy[Rise] = `*J >> T -> T >> *T >> J`
  case object `*J >> T -> T >> *T >> J` extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = e match {
      case App(
      Transpose(),
      App(App(Map(), Join()), y)
      ) =>
        Success((typed(y) |> transpose |> map(transpose) |> join) :: e.t)
      case _ => Failure(mapJoinBeforeTranspose)
    }
    override def toString = "mapJoinBeforeTranspose"
  }

  // join + join

  def joinBeforeJoin: Strategy[Rise] = `J >> J -> *J >> J`
  case object `J >> J -> *J >> J` extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = e match {
      case App(
      Join(),
      App(Join(), y)
      ) =>
        Success((typed(y) |> map(join) >> join) :: e.t)
      case _ => Failure(joinBeforeJoin)
    }
    override def toString = "joinBeforeJoin"
  }

  def mapJoinBeforeJoin: Strategy[Rise] = `*J >> J -> J >> J`
  case object `*J >> J -> J >> J` extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = e match {
      case App(
      Join(),
      App(App(Map(), Join()), y)
      ) =>
        Success((typed(y) |> join |> join) :: e.t)
      case _ => Failure(mapJoinBeforeJoin)
    }
    override def toString = "mapJoinBeforeJoin"
  }

  // split + slide

  def slideBeforeSplit: Strategy[Rise] = `slide(n)(s) >> split(k) -> slide(k+n-s)(k) >> map(slide(n)(s))`
  case object `slide(n)(s) >> split(k) -> slide(k+n-s)(k) >> map(slide(n)(s))` extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = e match {
      case App(
      DepApp(Split(), k: Nat),
      App(DepApp(DepApp(Slide(), n: Nat), s: Nat), y)
      ) =>
        Success((typed(y) |> slide(k + n - s)(k) |> map(slide(n)(s))) :: e.t)
      case _ => Failure(slideBeforeSplit)
    }
    override def toString = "slideBeforeSplit"
  }

  // TODO: what if s != 1?
  // slide(n)(s=1) >> slide(m)(k) -> slide(m+n-1)(k) >> map(slide(n)(1))
  case object slideBeforeSlide extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = e match {
      case App(
      DepApp(DepApp(Slide(), m: Nat), k: Nat),
      App(DepApp(DepApp(Slide(), n: Nat), s: Nat), in)
      ) if s == (1: Nat) =>
        Success((typed(in) |> slide(m+n-s)(k) |> map(slide(n)(s))) :: e.t)
      case _ => Failure(slideBeforeSplit)
    }
    override def toString = "slideBeforeSplit"
  }

  // nested map + reduce

  // different variants for rewriting map(reduce) to reduce(map)
  // todo what makes them different? can we decompose them into simpler rules?
  case object liftReduce extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = e match {

       // 2D array of pairs ----------------------------------------------------
      case App(Map(), Lambda(_,
      App(App(App(ReduceX(), op),
      _), _)
      // PairType is new here
      )) ::: FunType(ArrayType(_, ArrayType(_,PairType(_,_))), resultT) =>

        val result: TDSL[Rise] = (fun(x =>
          (reduceSeq(fun((acc, y) =>
            map(fun(a => typed(op)(a._1)(a._2))) $ zip(acc,y)
          ))(x._1 :: resultT) o // x._1 :: 2D array
            // transpose2D
            transpose o map(transpose) $ x._2)) o
          // unzip2D
          unzip o map(unzip)) :: e.t

        Success(result)

        // yet another case ----------------------------------------------------
      /*
    documentation for the concrete case I had:
    32.(float, 4.(float,float)) -> 32.float (via map(f o reduce))
    (32.float, 32.4.(float,float)) [unzip]
    (32.float, 4.32.(float,float)) [transpose $ x._2]
    now reducing (map(+)) x._2, using x_.1 as init,
     */
      case App(Map(), Lambda(_,
      App(_, // <- this function contained add, do I need this? ...
      // or can we get rid of this somehow?
      App(App(App(ReduceX(), op), _), _))
      // PairType  -> I need to be able to unzip
      // ArrayType -> I need to be able to transpose x._2
      ) ::: FunType(PairType(_,ArrayType(_,_)), _) // lambda.t
      ) ::: FunType(ArrayType(_,_), resultT) =>    // outermost app.t

        val result: TDSL[Rise] =
          (fun(x =>
            reduceSeq(
              fun((acc, y) => // acc::32.float, y::32.(float,float)
                map(fun(a => typed(op)(a._1)(a._2))) $ zip(acc,y)
              )
            )(x._1 :: resultT) o
              transpose $ x._2) o unzip) :: e.t

        Success(result)

        // "usual" case below --------------------------------------------------
        // this case already works for multiple dimensions (taken from old repo)
      case App(Map(), Lambda(mapVar,
           App(App(App(rx@(Reduce() | ReduceSeq()), op),
           init ::: (dt: DataType)), reduceArg)
      )) ::: FunType(inputT@ArrayType(size, ArrayType(_,_)), _) =>

      def reduceMap(zippedMapArg : (TDSL[Rise], TDSL[Rise]) => TDSL[Rise],
                    reduceArgFun: TDSL[Rise]): RewriteResult[Rise] = {
          Success((
            untyped(rx)(fun((acc, y) =>
              map(fun(x => app(app(op, fst(x)), snd(x)))) $ zippedMapArg(acc, y)
            ))(generate(fun(IndexType(size) ->: dt)(_ => init))) o reduceArgFun
          ) :: e.t)
        }

        reduceArg match {
          // simple case (see test "lift reduce")
          case x if x == mapVar =>
            reduceMap(
              (acc, y) => zip(acc, y),
              transpose
            )
          // zipped input (see test "MM to MM-LoopMKN")
          case App(App(Zip(), u), v) =>
            val notToBeTransposed = if (mapVar == u) v else u
            reduceMap(
              zippedMapArg = (acc, y) =>
                zip(acc, map(fun(bs => pair(bs, fst(y)))) $ snd(y)),
              reduceArgFun = zip(notToBeTransposed) o transpose
            )

          // expression is not in RNF!
          case a@App(_,_) => ???

          case _ =>
            // todo implement recursively
            val reduceArgTransposed = inputT match {
              case ArrayType(_, ArrayType(_, ArrayType(_,ArrayType(_,_)))) =>
                transpose o map(transpose) o map(map(transpose))
              case ArrayType(_, ArrayType(_, ArrayType(_,_))) =>
                transpose o map(transpose)
              case ArrayType(_, ArrayType(_,_)) => transpose
              case _ => ???
            }

            val result = reduceMap(
              (acc, y) => zip(acc, y),
              reduceArgTransposed
            )
            result
        }

      case _ => Failure(liftReduce)
    }
    override def toString = "liftReduce"
  }

  // mapSnd f >> mapFst g -> mapFst g >> mapSnd f
  def mapFstBeforeMapSnd: Strategy[Rise] = {
    case e @ App(App(MapFst(), g), App(App(MapSnd(), f), in)) =>
      Success(mapSnd(f)(mapFst(g, in)) :: e.t)
    case _ => Failure(mapFstBeforeMapSnd)
  }
}
