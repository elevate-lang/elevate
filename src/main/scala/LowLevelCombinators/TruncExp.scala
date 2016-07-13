package LowLevelCombinators

import Core.OperationalSemantics._
import Core._

import scala.xml.Elem

final case class TruncExp(n: Nat,
                          m: Nat,
                          dt: DataType,
                          array: Phrase[ExpType])
  extends LowLevelExpCombinator {

  override lazy val `type` = exp"[$m.$dt]"

  override def typeCheck(): Unit = {
    import TypeChecker._
    (n: Nat) -> (m: Nat) -> (dt: DataType) ->
      (array `:` exp"[$n.$dt]") -> `type`
  }

  override def inferTypes: TruncExp = {
    import TypeInference._
    val array_ = TypeInference(array)
    array_.t match {
      case ExpType(ArrayType(n_, dt_)) =>
        TruncExp(n_, m, dt_, array_)
      case x => error(x.toString, "ArrayType")
    }
  }

  override def eval(s: Store): Data = ???

  override def visitAndRebuild(fun: VisitAndRebuild.fun): Phrase[ExpType] = {
    TruncExp(fun(n), fun(m), fun(dt), VisitAndRebuild(array, fun))
  }

  override def rewriteToImperativeAcc(A: Phrase[AccType]): Phrase[CommandType] = ???

  override def rewriteToImperativeExp(C: Phrase[->[ExpType, CommandType]]): Phrase[CommandType] = ???

  override def xmlPrinter: Elem =
    <truncExp n={n.toString} m={m.toString} dt={dt.toString}>
      {Core.xmlPrinter(array)}
    </truncExp>

  override def prettyPrint: String = s"(truncExp $array)"

}