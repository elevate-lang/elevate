package idealised.DPIA.FunctionalPrimitives

import idealised.DPIA.Compilation.{TranslationContext, TranslationToImperative}
import idealised.DPIA.DSL._
import idealised.DPIA.ImperativePrimitives.AsVectorAcc
import idealised.DPIA.Phrases._
import idealised.DPIA.Semantics.OperationalSemantics._
import idealised.DPIA.Types._
import idealised.DPIA._

import scala.xml.Elem

final case class AsVector(n: Nat,
                          m: Nat,
                          dt: ScalarType,
                          array: Phrase[ExpType])
  extends ExpPrimitive {

  override val t: ExpType =
    (n: Nat) ->: (m: Nat) ->: (dt: ScalarType) ->:
      (array :: exp"[${m * n}.$dt]") ->:
        exp"[$m.${VectorType(n, dt)}]"

  override def visitAndRebuild(f: VisitAndRebuild.Visitor): Phrase[ExpType] = {
    AsVector(f.nat(n), f.nat(m), f.data(dt), VisitAndRebuild(array, f))
  }

  override def eval(s: Store): Data = ???

  override def prettyPrint: String = s"(asVector ${n.toString} ${PrettyPhrasePrinter(array)})"

  override def xmlPrinter: Elem =
    <asVector n={ToString(n)}>
      {Phrases.xmlPrinter(array)}
    </asVector>

  override def acceptorTranslation(A: Phrase[AccType])
                                  (implicit context: TranslationContext): Phrase[CommType] = {
    import TranslationToImperative._

    acc(array)(AsVectorAcc(n, m, dt, A))
  }

  override def mapAcceptorTranslation(f: Phrase[ExpType ->: ExpType], A: Phrase[AccType])
                                     (implicit context: TranslationContext): Phrase[CommType] =
    ???

  override def continuationTranslation(C: Phrase[->:[ExpType, CommType]])
                                      (implicit context: TranslationContext): Phrase[CommType] = {
    TranslationToImperative.con(array)(λ(array.t)(x => C(AsVector(n, m, dt, x)) ))
  }
}
