package elevate.rise

import elevate.core.strategies.predicate._
import elevate.core.strategies.traversal._
import elevate.rise.rules.traversal._
import elevate.core.{Failure, RewriteResult, Strategy, Success}
import rise.core._
import rise.core.types._
import rise.core.TypedDSL._

package object rules {

  case object betaReduction extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = e match {
      case App(Lambda(x, b), v) =>
        Success(substitute.exprInExpr(v, `for` = x, in = b))
      case DepApp(DepLambda(x, b), v) =>
        Success(substitute.kindInExpr(v, `for` = x, in = b))
      case _ => Failure(betaReduction)
    }
    override def toString = "betaReduction"
  }

  def containsAtLeast(n: Int, x: Rise): Strategy[Rise] =
    skip(n)(isEqualTo(x))

  // TODO: express as a combination of strategies
  case object gentleBetaReduction extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = e match {
      case App(Lambda(x, b), v: Identifier) =>
        Success(substitute.exprInExpr(v, `for` = x, in = b))
      case App(Lambda(x, b), v) if !containsAtLeast(1, x)(b) =>
        Success(substitute.exprInExpr(v, `for` = x, in = b))
      case DepApp(DepLambda(x, b), v) =>
        Success(substitute.kindInExpr(v, `for` = x, in = b))
      case _ => Failure(gentleBetaReduction)
    }
    override def toString = "gentleBetaReduction"
  }

  case object etaReduction extends Strategy[Rise]  {
    def apply(e: Rise): RewriteResult[Rise] = e match {
      case Lambda(x1, App(f, x2)) if x1 == x2 && !contains[Rise](x1).apply(f) => Success(f :: e.t)
      case _                                                                  => Failure(etaReduction)
    }
    override def toString = "etaReduction"
  }

  case object etaAbstraction extends Strategy[Rise] {
    def apply(f: Rise): RewriteResult[Rise] = f.t match {
      case FunType(_, _) =>
        val x = identifier(freshName("η"))
        Success(lambda(x, app(f, x)) :: f.t)
      case _ => Failure(etaAbstraction)
    }
    override def toString = "etaAbstraction"
  }

  case object idxReduction extends Strategy[Rise] {
    import rise.core.primitives._
    import rise.core.semantics._
    import arithexpr.arithmetic._

    def isMakeArray(e: Rise): Boolean = e match {
      case MakeArray(_) => true
      case App(f, _) => isMakeArray(f)
      case _ => false
    }

    def indexMakeArray(e: Rise, i: Long, n: Long): Rise = e match {
      case App(_, v) if i == (n - 1) => v
      case App(f, _) => indexMakeArray(f, i, n - 1)
      case _ => throw new Exception("index out of bounds")
    }

    def apply(e: Rise): RewriteResult[Rise] = e match {
      case App(App(Idx(), Literal(IndexData(Cst(i), Cst(n)))), mka)
        if isMakeArray(mka) =>
        Success(indexMakeArray(mka, i, n))
      case _ =>
        Failure(idxReduction)
    }
  }

  // todo: remove once all rules are type-preserving
  case object inferRise extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = Success(infer(e))
  }

}
