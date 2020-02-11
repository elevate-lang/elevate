package elevate

import _root_.rise.core._
import _root_.rise.core.types.Type
import _root_.rise.core.primitives.{Reduce, ReduceSeq}

package object rise {
  type Rise = Expr

  // type-extractor
  object :: {
    def unapply(e: Expr): Option[(Expr, Type)] = Some((e, e.t))
  }

  object ReduceX {
    def unapply(e: Expr): Boolean = e match {
      case Reduce() => true
      case ReduceSeq() => true
      case _ => false
    }
  }
}
