package idealised.DPIA.FunctionalPrimitives

import idealised.DPIA.Compilation.RewriteToImperative
import idealised.DPIA.DSL._
import idealised.DPIA.ImperativePrimitives.MapWrite
import idealised.DPIA.IntermediatePrimitives.AbstractMapI
import idealised.DPIA.Phrases._
import idealised.DPIA.Types._
import idealised.DPIA._

final case class MapOut(n: Nat,
                        dt1: DataType,
                        dt2: DataType,
                        f: Phrase[ExpType -> ExpType],
                        input: Phrase[ExpType])
  extends AbstractMap(n, dt1, dt2, f, input)
{
  override def makeMap: (Nat, DataType, DataType, Phrase[ExpType -> ExpType], Phrase[ExpType]) => AbstractMap = Map

  override def makeMapI: (Nat, DataType, DataType, Phrase[ExpType -> (AccType -> CommandType)], Phrase[ExpType], Phrase[AccType]) => AbstractMapI = ???

  override def acceptorTranslation(A: Phrase[AccType]): Phrase[CommandType] = {
    import RewriteToImperative._

    ???
    // acc(input)(MapWrite(n, dt2, dt1, λ(acc"[$dt1]")(o => acc(f(input))(o)), A))
  }

  override def continuationTranslation(C: Phrase[ExpType -> CommandType]): Phrase[CommandType] = {
    import RewriteToImperative._

    ???
  }
}