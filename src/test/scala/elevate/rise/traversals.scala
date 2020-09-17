package elevate.rise

import elevate.core.strategies.basic._
import elevate.core.strategies.traversal._
import elevate.rise.meta.fission.bodyFission
import elevate.rise.meta.traversal.inBody
import elevate.rise.rules.algorithmic._
import elevate.rise.rules.movement._
import elevate.rise.rules.traversal.{argument, argumentOf, body, function, _}
import elevate.rise.rules.traversal.default._
import elevate.rise.strategies.util._
import elevate.util._
import rise.core.TypedDSL._
import rise.core.types.NatKind
import rise.core.primitives._


class traversals extends elevate.test_util.Tests {

  def tileND = elevate.rise.strategies.tiling.tileND(RiseTraversable)
  val DFNF = elevate.rise.strategies.normalForm.DFNF()(RiseTraversable)
  val RNF = elevate.rise.strategies.normalForm.RNF()(RiseTraversable)
  val FNF = elevate.rise.meta.fission.FNF(elevate.rise.meta.traversal.MetaRiseTraversable(RiseTraversable))

  test("rewrite simple elevate strategy") {
    val expr = fun(f => fun(g => map(f) >> map(g)))
    val strategy = body(body(body(mapFusion `;` function(mapLastFission()))))

    val metaStrategy = inBody(inBody(bodyFission))(strategy)
    val newStrategy = metaStrategy.get
    assert(strategy != newStrategy)
    assert(makeClosed(newStrategy(expr).get) == makeClosed(strategy(expr).get))
  }

  test("simplification") {
    val input2D = λ(i => λ(f => **!(f) $ i))
    val orig = body(body(tileND(2)(tileSize)))
    println(orig.toString)

    val oldTiling = body(body(
      function(argumentOf(Map()(), body(function(splitJoin(4)) `;` DFNF `;` RNF))) `;`
        function(splitJoin(4)) `;`
        DFNF `;` RNF `;` DFNF `;` RNF `;` DFNF `;`
        argument(argument(function(argumentOf(Map()(), body(idAfter `;` createTransposePair `;` DFNF `;` argument(mapMapFBeforeTranspose())))) `;` DFNF `;` RNF)) `;`
        DFNF `;` RNF `;` DFNF `;` RNF `;` RNF
    ))

    val simplified = body(body(
      function(argumentOf(Map()(), body(function(splitJoin(4))))) `;`
        function(splitJoin(4)) `;`
        RNF `;` DFNF `;`
        argument(argument(function(argumentOf(Map()(), body(idAfter `;` createTransposePair `;` DFNF `;` argument(mapMapFBeforeTranspose())))) `;` RNF))))

    val normalized = FNF(simplified).get
    println(normalized)
    val normalizedModified = body(body(function(argumentOf(Map()(), body(function(splitJoin(4))))))) `;`
    inferType `;`
      body(body(function(splitJoin(4)))) `;`
      inferType `;`
      body(body(RNF)) `;`
      inferType `;`
      body(body(DFNF)) `;`
      inferType `;`
      body(body(argument(argument(function(argumentOf(Map()(), body(idAfter))))))) `;`
      inferType `;`
      body(body(argument(argument(function(argumentOf(Map()(), body(createTransposePair))))))) `;`
      inferType `;`
      body(body(argument(argument(function(argumentOf(Map()(), body(DFNF))))))) `;`
      inferType `;`
      body(body(argument(argument(function(argumentOf(Map()(), body(argument(mapMapFBeforeTranspose())))))))) `;`
      inferType `;`
      body(body(argument(argument(RNF))))


    assert(makeClosed(oldTiling(input2D).get) == makeClosed(simplified(input2D).get))
    assert(makeClosed(oldTiling(input2D).get) == makeClosed(normalizedModified(input2D).get))
  }

  test("RNF did not normalize") {
    val expr2 = lambda(identifier("ee1"), lambda(identifier("ee2"), app(join, app(app(map, lambda(identifier("η125"), app(app(map, lambda(identifier("ee3"), app(join, app(app(map, lambda(identifier("η124"), app(app(map, lambda(identifier("η123"), app(identifier("ee2"), identifier("η123")))), identifier("η124")))), app(depApp[NatKind](split, 4), identifier("ee3")))))), identifier("η125")))), app(depApp[NatKind](split, 4), identifier("ee1"))))))
    val expr5 = lambda(identifier("ee1"), lambda(identifier("ee2"), app(join, app(app(map, lambda(identifier("η141"), app(app(map, lambda(identifier("η140"), app(join, identifier("η140")))), identifier("η141")))), app(app(map, lambda(identifier("η145"), app(app(map, lambda(identifier("η144"), app(app(map, lambda(identifier("η143"), app(app(map, lambda(identifier("η142"), app(identifier("ee2"), identifier("η142")))), identifier("η143")))), identifier("η144")))), identifier("η145")))), app(app(map, lambda(identifier("η147"), app(app(map, lambda(identifier("η146"), app(depApp[NatKind](split, 4), identifier("η146")))), identifier("η147")))), app(depApp[NatKind](split, 4), identifier("ee1"))))))))

    assert(makeClosed(RNF(expr2).get) == makeClosed(toExpr(expr5)))
  }

  test("id traversals") {
    val expr = fun(f => fun(g => map(f) >> map(g)))
    val id = elevate.core.strategies.basic.id[Rise]

    assert(
      List(
        allTopdown(id).apply(expr),
        allBottomup(id).apply(expr),
        downup(id).apply(expr),
        downup2(id,id).apply(expr),
        topDown(id).apply(expr),
        bottomUp(id).apply(expr),
        alltd(id).apply(expr),
        sometd(id).apply(expr),
        somebu(id).apply(expr)
      ).forall(x => betaEtaEquals(x.get, expr))
    )
  }

  test("simple fusion") {
    val expr = fun(f => fun(g => map(f) >> map(g)))
    val gold = fun(f => fun(g => map(f >> g)))

    assert(
      List(
        topDown(mapFusion).apply(expr),
        bottomUp(mapFusion).apply(expr),
        alltd(mapFusion).apply(expr),
        sometd(mapFusion).apply(expr),
        somebu(mapFusion).apply(expr),
        allTopdown(`try`(mapFusion)).apply(expr),
        allBottomup(`try`(mapFusion)).apply(expr)
      ).forall(x => betaEtaEquals(x.get, gold))
    )

  }
}
