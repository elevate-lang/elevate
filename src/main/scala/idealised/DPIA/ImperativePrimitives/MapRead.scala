package idealised.DPIA.ImperativePrimitives

import idealised.DPIA._
import idealised.DPIA.DSL._
import idealised.DPIA.Types._
import idealised.DPIA.Phrases._
import idealised.DPIA.Semantics.OperationalSemantics._

import scala.language.reflectiveCalls

final case class MapRead(n: Nat,
                         dt1: DataType,
                         dt2: DataType,
                         f: Phrase[ExpType -> ((ExpType -> CommandType) -> CommandType)],
                         input: Phrase[ExpType])
  extends ExpPrimitive
{
  override val `type`: ExpType =
    (n: Nat) -> (dt1: DataType) -> (dt2: DataType) ->
      (f :: exp"[$dt1]" -> (t"exp[$dt2] -> comm" -> comm)) ->
      (input :: exp"[$n.$dt1]") -> exp"[$n.$dt2]"

  override def visitAndRebuild(v: VisitAndRebuild.Visitor): Phrase[ExpType] = {
    MapRead(v(n), v(dt1), v(dt2), VisitAndRebuild(f, v), VisitAndRebuild(input, v))
  }

  override def eval(s: Store): Data = ???

  override def acceptorTranslation(A: Phrase[AccType]): Phrase[CommandType] =
    throw new Exception("This should not happen")

  override def continuationTranslation(C: Phrase[->[ExpType, CommandType]]): Phrase[CommandType] =
    throw new Exception("This should not happen")

  override def prettyPrint: String = s"(mapRead $f $input)"

  override def xmlPrinter: xml.Elem =
    <mapRead n={ToString(n)} dt1={ToString(dt1)} dt2={ToString(dt2)}>
      <f>
        {Phrases.xmlPrinter(f)}
      </f>
      <input>
        {Phrases.xmlPrinter(input)}
      </input>
    </mapRead>
}
