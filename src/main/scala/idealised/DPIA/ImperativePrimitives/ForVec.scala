package idealised.DPIA.ImperativePrimitives

import idealised.DPIA.DSL._
import idealised.DPIA.FunctionalPrimitives.AsIndex
import idealised.DPIA.Phrases._
import idealised.DPIA.Semantics.OperationalSemantics
import idealised.DPIA.Semantics.OperationalSemantics._
import idealised.DPIA.Types._
import idealised.DPIA._

import scala.xml.Elem

final case class ForVec(n: Nat,
                        dt: ScalarType,
                        out: Phrase[AccType],
                        body: Phrase[ExpType -> (AccType -> CommType)])
  extends CommandPrimitive
{
  override val t: CommType =
    (n: Nat) -> (dt: ScalarType) ->
      (out :: acc"[${VectorType(n, dt)}]") ->
        (body :: t"exp[idx($n)] -> acc[$dt] -> comm") ->
          comm

  override def eval(s: Store): Store = {
    val nE = evalIndexExp(s, AsIndex(n, Natural(n)))
    val bodyE = OperationalSemantics.eval(s, body)(OperationalSemantics.BinaryFunctionEvaluator)

    (0 until nE.eval).foldLeft(s)((s1, i) => {
      OperationalSemantics.eval(s1,
        bodyE(Literal(i))(out `@` Literal(i)))
    })
  }

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): Phrase[CommType] = {
    ForVec(fun(n), fun(dt), VisitAndRebuild(out, fun), VisitAndRebuild(body, fun))
  }

  override def prettyPrint: String =
    s"(ForVec $n ${PrettyPhrasePrinter(out)} ${PrettyPhrasePrinter(body)})"


  override def xmlPrinter: Elem =
    <forVec n={ToString(n)} dt={ToString(dt)}>
      <output type={ToString(AccType(VectorType(n, dt)))}>
        {Phrases.xmlPrinter(out)}
      </output>
      <body type={ToString(ExpType(IndexType(n)) -> (AccType(dt) -> CommType()))}>
        {Phrases.xmlPrinter(body)}
      </body>
    </forVec>
}
