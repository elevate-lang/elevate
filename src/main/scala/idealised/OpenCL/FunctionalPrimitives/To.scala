package idealised.OpenCL.FunctionalPrimitives

import idealised.DPIA.Compilation.{TranslationContext, TranslationToImperative}
import idealised.DPIA.DSL.{`new` => _, _}
import idealised.DPIA.Phrases._
import idealised.DPIA.Semantics.OperationalSemantics
import idealised.DPIA.Semantics.OperationalSemantics.{Data, Store}
import idealised.DPIA.Types._
import idealised.DPIA.{Phrases, _}
import idealised.OpenCL.DSL.`new`

import scala.xml.Elem

final case class To(addrSpace: AddressSpace,
                    dt: DataType,
                    input: Phrase[ExpType])
  extends ExpPrimitive {

  override val t: ExpType =
    (addrSpace : AddressSpace) ->: (dt: DataType) ->:
      (input :: exp"[$dt, $write]") ->: exp"[$dt, $read]"

  override def eval(s: Store): Data = OperationalSemantics.eval(s, input)

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): Phrase[ExpType] = {
    To(fun.addressSpace(addrSpace), fun.data(dt), VisitAndRebuild(input, fun))
  }

  override def prettyPrint: String =
    s"(to$addrSpace ${PrettyPhrasePrinter(input)})"

  override def xmlPrinter: Elem =
    <to addrSpace={ToString(addrSpace)} dt={ToString(dt)}>
      <input type={ToString(ExpType(dt, write))}>
        {Phrases.xmlPrinter(input)}
      </input>
    </to>.copy(label = {
      val name = this.getClass.getSimpleName
      Character.toLowerCase(name.charAt(0)) + name.substring(1)
    })

  override def acceptorTranslation(A: Phrase[AccType])
                                  (implicit context: TranslationContext): Phrase[CommType] = {
    ???
  }

  override def continuationTranslation(C: Phrase[->:[ExpType, CommType]])
                                      (implicit context: TranslationContext): Phrase[CommType] = {
    import TranslationToImperative._

    `new`(addrSpace)(dt, tmp => acc(input)(tmp.wr) `;` C(tmp.rd) )
  }
}
