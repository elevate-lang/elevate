package LowLevelCombinators

import Core.OperationalSemantics._
import Core._
import DSL.typed._

import scala.xml.Elem

final case class For(n: Nat,
                     body: Phrase[ExpType -> CommandType])
  extends LowLevelCommCombinator {

  override def typeCheck(): Unit = {
    import TypeChecker._
    (n: Nat) -> (body `:` t"exp[idx($n)] -> comm") -> comm
  }

  override def eval(s: Store): Store = {
    val nE = evalIndexExp(s, LiteralPhrase(IndexData(n)))
    val bodyE = OperationalSemantics.eval(s, body)
    (0 until nE.eval).foldLeft(s)((s1, i) =>
      OperationalSemantics.eval(s1, bodyE(LiteralPhrase(i)))
    )
  }

  override def visitAndRebuild(fun: VisitAndRebuild.fun): Phrase[CommandType] = {
    For(fun(n), VisitAndRebuild(body, fun))
  }

  override def prettyPrint: String = s"(for 0..$n ${PrettyPrinter(body)})"

  override def xmlPrinter: Elem =
    <for n={ToString(n)}>
      {Core.xmlPrinter(body)}
    </for>
}