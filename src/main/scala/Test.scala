import PhraseType._

object Test extends App {

  val v1 = identifier("v1")
  v1.t = ExpType(int)
  val l = Lambda(v1, v1)

  println(TypeChecker( l ))

  val v2 = identifier("v2")
  v2.t = ExpType(int)
  println(TypeChecker( l(v2) ))

  val one = IntLiteral(1)
  val two = IntLiteral(2)
  val add = BinOp(BinOp.Op.ADD, one, two)
  println(TypeChecker(add))

  val l2 = Lambda(v1, BinOp(BinOp.Op.ADD, one, v1))
  println(TypeChecker(Apply(l2, two)))

  val p = Pair(v1, one)
  println(TypeChecker( p ))

}
