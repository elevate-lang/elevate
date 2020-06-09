package elevate.rise

import elevate.core.strategies.predicate._
import elevate.core.strategies.traversal._
import elevate.rise.rules.traversal._
import elevate.core.{Failure, RewriteResult, Strategy, Success}
import elevate.macros.RuleMacro.rule
import rise.core._
import rise.core.types._
import rise.core.TypedDSL._

package object rules {

  @rule def betaReduction: Strategy[Rise] = {
    case App(Lambda(x, b), v) =>
      Success(substitute.exprInExpr(v, `for` = x, in = b))
    case DepApp(DepLambda(x, b), v) =>
      Success(substitute.kindInExpr(v, `for` = x, in = b))
  }

  @rule def containsAtLeast(n: Int, x: Rise): Strategy[Rise] =
    skip(n)(isEqualTo(x))

  // TODO: express as a combination of strategies
  @rule def gentleBetaReduction: Strategy[Rise] = {
    case App(Lambda(x, b), v: Identifier) =>
      Success(substitute.exprInExpr(v, `for` = x, in = b))
    case App(Lambda(x, b), v @ App(App(primitives.Pair(), _), _)) =>
      Success(substitute.exprInExpr(v, `for` = x, in = b))
    case App(Lambda(x, b), v) if !containsAtLeast(1, x)(b) =>
      Success(substitute.exprInExpr(v, `for` = x, in = b))
    case DepApp(DepLambda(x, b), v) =>
      Success(substitute.kindInExpr(v, `for` = x, in = b))
  }

  @rule def etaReduction: Strategy[Rise] = {
    case e@Lambda(x1, App(f, x2)) if x1 == x2 && !contains[Rise](x1).apply(f) => Success(f :: e.t)
  }

  @rule def etaAbstraction: Strategy[Rise] = f => f.t match {
    case FunType(_, _) =>
      val x = identifier(freshName("η"))
      Success(lambda(x, app(f, x)) :: f.t)
    case _ => Failure(etaAbstraction)
  }

  @rule def idxReduction: Strategy[Rise] = e => {
    import rise.core.primitives._
    import rise.core.semantics._
    import arithexpr.arithmetic._

    @scala.annotation.tailrec
    def isMakeArray(e: Rise): Boolean = e match {
      case MakeArray(_) => true
      case App(f, _) => isMakeArray(f)
      case _ => false
    }

    @scala.annotation.tailrec
    def indexMakeArray(e: Rise, i: Long, n: Long): Rise = e match {
      case App(_, v) if i == (n - 1) => v
      case App(f, _) => indexMakeArray(f, i, n - 1)
      case _ => throw new Exception("index out of bounds")
    }

    e match {
      case App(App(Idx(), Literal(IndexData(Cst(i), Cst(n)))), mka)
        if isMakeArray(mka) =>
        Success(indexMakeArray(mka, i, n))
      case _ =>
        Failure(idxReduction)
    }
  }

  // todo: remove once all rules are type-preserving
  @rule def inferRise: Strategy[Rise] = e => Success(infer(e))
}
