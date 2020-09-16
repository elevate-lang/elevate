package elevate.rise.strategies

import elevate.core.strategies.Traversable
import elevate.core.{Failure, Strategy, Success}
import elevate.core.strategies.traversal.tryAll
import elevate.rise.Rise
import rise.core.TypedDSL._
import rise.core._

object lowering {
  def storeInMemory(what: Strategy[Rise], how: Strategy[Rise])
                   (implicit ev: Traversable[Rise]): Strategy[Rise] = { p =>
    extract(what)(p) >>= (extracted => {
      how(extracted) >>= (storedSubExpr => {
        val idx = Identifier(freshName("x"))(extracted.t)

        replaceAll(what, idx)(ev)(p) match {
          case Success(replaced) =>
            Success(let(toMem(storedSubExpr))(lambda(TDSL(idx), replaced)) :: p.t)
          case Failure(_) => Failure(storeInMemory(what, how))
        }
      })
    })
  }


  def replaceAll(exprPredicate: Strategy[Rise], withExpr: Rise)
                (implicit ev: Traversable[Rise]): Strategy[Rise] =
    p => tryAll(exprPredicate `;` insert(withExpr)).apply(p)

  def insert(expr: Rise): Strategy[Rise] = _ => Success(expr)
  def extract(what: Strategy[Rise]): Strategy[Rise] = (expr: Rise) => {
    what(expr).flatMapFailure(_ => expr match {
      case App(f,e)        => extract(what)(f).flatMapFailure(_ => extract(what)(e))
      case Lambda(x, e)    => extract(what)(x).flatMapFailure(_ => extract(what)(e))
      case DepLambda(_, e) => extract(what)(e)
      case DepApp(_, _)       => Failure(extract(what))
      case _: Identifier      => Failure(extract(what))
      case _: Literal         => Failure(extract(what))
      case _: ForeignFunction => Failure(extract(what))
      case _: Primitive       => Failure(extract(what))
    })
  }
}
