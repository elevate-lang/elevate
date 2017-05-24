package idealised.OpenCL.FunctionalPrimitives

import idealised.DPIA._
import idealised.DPIA.Types.{DataType, ExpType}
import idealised.DPIA.Phrases.Phrase
import idealised.DPIA.FunctionalPrimitives.AbstractMap
import idealised.OpenCL.IntermediatePrimitives.MapWorkGroupI

final case class MapWorkGroup(n: Nat,
                              dt1: DataType,
                              dt2: DataType,
                              f: Phrase[ExpType -> ExpType],
                              array: Phrase[ExpType])
  extends AbstractMap(n, dt1, dt2, f, array) {
  override def makeMap = MapWorkGroup

  override def makeMapI = MapWorkGroupI
}
