package idealised.OpenCL.IntermediatePrimitives

import idealised.DPIA.DSL.{`new` => _, _}
import idealised.DPIA.Compilation.TranslationContext
import idealised.DPIA.Compilation.TranslationToImperative.acc
import idealised.DPIA.Phrases._
import idealised.DPIA.Types._
import idealised.DPIA._
import idealised.OpenCL.DSL._

import scala.language.reflectiveCalls

object OpenCLReduceSeqI {
  def apply(n: Nat,
            initAddrSpace: idealised.DPIA.Types.AddressSpace,
            dt1: DataType, dt2: DataType,
            f: Phrase[ExpType ->: ExpType ->: AccType ->: CommType],
            init: Phrase[ExpType],
            in: Phrase[ExpType],
            out: Phrase[ExpType ->: CommType],
            unroll: Boolean)
           (implicit context: TranslationContext): Phrase[CommType] = {
    comment("oclReduceSeq") `;`
      `new`(initAddrSpace)(dt2, accumulator =>
        acc(init)(accumulator.wr) `;`
          `for`(n, i => f(accumulator.rd)(in `@` i)(accumulator.wr), unroll) `;`
          out(accumulator.rd)
      )
  }
}
