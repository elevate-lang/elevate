package idealised.DPIA

import lift.core._
import lift.core.DSL._
import lift.core.types._
import lift.core.primitives._
import lift.core.semantics.NatData
import idealised.util.gen

class NatExpr extends idealised.util.Tests {
  test("Nat can be used as DataType inside of an expression in C.") {
    gen.CProgram(fun(NatType)(n => n + Literal(NatData(1))))
  }

  test("asNat acceptor translation is working correctly") {
    gen.CProgram(fun(IndexType(4))(i => indexAsNat(i)))
  }

  test("AsNat and plus operation generates syntactically correct code in C.") {
    gen.CProgram(nFun(n => fun(IndexType(n))(i => indexAsNat(i) + n)))
  }

  test("Nat is implicitly converted to NatExpr in an expression.") {
    gen.CProgram(nFun(n => fun(IndexType(n))(i => indexAsNat(i) + n)))
  }

  test("asIndex acceptor translation is working correctly.") {
    gen.CProgram(nFun(n => fun(NatType)(i => natAsIndex(n)(i))))
  }
}
