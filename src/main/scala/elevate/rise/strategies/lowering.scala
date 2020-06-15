package elevate.rise.strategies

import elevate.core.{Failure, Strategy, Success}
import elevate.core.strategies.traversal.tryAll
import elevate.rise.Rise
import rise.core.TypedDSL.{TDSL, lambda, let}
import rise.core._
import elevate.rise.rules.traversal._

object lowering {

  def storeInMemory(what: Strategy[Rise],
                    how: Strategy[Rise]): Strategy[Rise] = { p =>
    extract(what)(p) >>= (extracted => {
      how(extracted) >>= (storedSubExpr => {
        val idx = Identifier(freshName("x"))(extracted.t)

        replaceAll(what, idx)(p) match {
          case Success(replaced) => Success(toMem(storedSubExpr)(lambda(TDSL(idx), replaced)))
          case Failure(_) => Failure(storeInMemory(what, how))
        }
      })
    })
  }


  def replaceAll(exprPredicate: Strategy[Rise], withExpr: Rise): Strategy[Rise] =
    p => tryAll(exprPredicate `;` insert(withExpr)).apply(p)

  def toMem(e: Rise)(f: TDSL[Lambda]): TDSL[App] = let(f)(e)
  def insert(expr: Rise): Strategy[Rise] = _ => Success(expr)
  def extract(what: Strategy[Rise]): Strategy[Rise] = (expr: Rise) => {
    what(expr).flatMapFailure(_ => expr match {
      case App(f,e)        => extract(what)(f).flatMapFailure(_ => extract(what)(e))
      case Lambda(x, e)    => extract(what)(x).flatMapFailure(_ => extract(what)(e))
      case DepLambda(x, e) => extract(what)(e)
      case _: Identifier      => Failure(extract(what))
      case _: Literal         => Failure(extract(what))
      case _: ForeignFunction => Failure(extract(what))
      case _: Primitive       => Failure(extract(what))
      case _ => ??? // forgot something?
    })
  }
}
