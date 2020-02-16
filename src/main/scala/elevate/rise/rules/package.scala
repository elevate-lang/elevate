package elevate.rise

import elevate.core.strategies.predicate._
import elevate.rise.rules.traversal._
import elevate.core.{Failure, RewriteResult, Strategy, Success}
import rise.core._
import rise.core.types._
import rise.core.TypedDSL._

package object rules {

  case object betaReduction extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = e match {
      case App(f, x) => typedLifting.liftFunExpr(f) match {
        case lifting.Reducing(lf) => Success(lf(x) :: e.t)
        case _                    => Failure(betaReduction)
      }
      case DepApp(f, n: Nat) => typedLifting.liftDepFunExpr[NatKind](f) match {
        case lifting.Reducing(lf) => Success(lf(n) :: e.t)
        case _                    => Failure(betaReduction)
      }
      case DepApp(f, dt: DataType) => typedLifting.liftDepFunExpr[DataKind](f) match {
        case lifting.Reducing(lf) => Success(lf(dt) :: e.t)
        case _                    => Failure(betaReduction)
      }
      case DepApp(f, addr: AddressSpace) => typedLifting.liftDepFunExpr[AddressSpaceKind](f) match {
        case lifting.Reducing(lf) => Success(lf(addr) :: e.t)
        case _ => Failure(betaReduction)
      }
      case DepApp(f, n2n: NatToNat) => typedLifting.liftDepFunExpr[NatToNatKind](f) match {
        case lifting.Reducing(lf) => Success(lf(n2n) :: e.t)
        case _ => Failure(betaReduction)
      }
      case DepApp(f, n2d: NatToData) => typedLifting.liftDepFunExpr[NatToDataKind](f) match {
        case lifting.Reducing(lf) => Success(lf(n2d) :: e.t)
        case _ => Failure(betaReduction)
      }
      case _                      => Failure(betaReduction)
    }
    override def toString = "betaReduction"
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

  // todo: remove once all rules are type-preserving
  case object inferRise extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = Success(infer(e))
  }

}
