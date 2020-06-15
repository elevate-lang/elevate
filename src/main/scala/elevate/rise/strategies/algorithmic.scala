package elevate.rise.strategies

import com.github.ghik.silencer.silent
import elevate.core.strategies.{Traversable, basic}
import elevate.core.strategies.basic.{applyNTimes, id}
import elevate.core.strategies.traversal._
import elevate.rise.strategies.traversal._
//import elevate.rise.rules.traversal.default._
import elevate.core.{Failure, RewriteResult, Strategy, Success}
import elevate.rise.Rise
import elevate.rise.rules.algorithmic.fuseReduceMap
import elevate.rise.rules.movement._
import elevate.rise.rules.traversal.{argument, argumentOf, body, function}
import elevate.rise.strategies.normalForm.{DFNF, RNF}
import elevate.rise.strategies.predicate.{isApplied, isMap, isReduceSeq}
import elevate.rise.strategies.traversal.fmap
import rise.core.TypedDSL._
import rise.core._
import rise.core.primitives.ReduceSeq

object algorithmic {
  // TODO: only compose simpler rules
  // TODO: what if 'x' is used in 'f'?

  // fission of the first function to be applied inside a map
  // *(g >> .. >> f) -> *g >> *(.. >> f)
  case object mapFirstFission extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = e match {
      case App(primitives.Map(), Lambda(x, gx)) => Success(mapFirstFissionRec(x, fun(e => e), gx) :: e.t)
      case _                                    => Failure(mapFirstFission)
    }

    // TODO: this should be expressed with elevate strategies
    @silent
    @scala.annotation.tailrec
    private def mapFirstFissionRec(x: Identifier, f: TDSL[Rise], gx: Rise): TDSL[Rise] = {
      gx match {
        case App(f2, gx2) =>
          if (gx2 == x) {
            map(f2) >> map(f)
          } else {
            mapFirstFissionRec(x, fun(e => f(typed(f2)(e))), gx2)
          }
      }
    }
  }

  // fission of all the functions chained inside a map
  // *(g >> .. >> f) -> *g >> .. >> *f
  case object mapFullFission extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = e match {
      case App(primitives.Map(), Lambda(x, gx)) => Success(mapFullFissionRec(x, gx) :: e.t)
      case _                                    => Failure(mapFullFission)
    }

    // TODO: this should be expressed with elevate strategies
    @silent
    def mapFullFissionRec(x: Identifier, gx: Rise): TDSL[Rise] = {
      gx match {
        case App(f, gx2) =>
          if (gx2 == x) {
            map(f)
          } else {
            mapFullFissionRec(x, gx2) >> map(f)
          }
      }
    }

  }

  //scalastyle:off
  def normForReorder(implicit ev: Traversable[Rise]) =
    (slideBeforeMap `@` topDown[Rise]) `;;`
    (fuseReduceMap `@` topDown[Rise]) `;;`
    (fuseReduceMap `@` topDown[Rise]) `;;` RNF()

  def reorder(l: List[Int])(implicit ev: Traversable[Rise]): Strategy[Rise] = normForReorder `;` (reorderRec(l) `@` topDown[Rise])
  case class reorderRec(l: List[Int])(implicit ev: Traversable[Rise]) extends Strategy[Rise] {
    val isFullyAppliedReduceSeq: Strategy[Rise] = isApplied(isApplied(isApplied(isReduceSeq))) <+
      argument(isApplied(isApplied(isApplied(isReduceSeq)))) // weird reduce special case
    val isFullyAppliedMap: Strategy[Rise] = isApplied(isApplied(isMap))

    // pos = how far nested is the reduction?
    def moveReductionUp(pos: Int): Strategy[Rise] = {
      if (pos <= 1) id[Rise]()
      else
        applyNTimes(pos-2)(stepDown)(function(liftReduce)) `;` DFNF() `;` RNF() `;`  moveReductionUp(pos-1)
    }
    def apply(e: Rise): RewriteResult[Rise] = l match {
      // nothing to reorder, go further down
      case x :: xs if x == 1 => (stepDown(reorderRec(xs.map(y => if (y>x) y-1 else y ))))(e)
      // work to do
      case pos :: xs => (

        (applyNTimes(pos-1)(stepDown)(isFullyAppliedReduceSeq) `;`
          // move reduction and normalize the AST
          moveReductionUp(pos) `;`
          // recurse and continue reordering
          stepDown(reorderRec(xs.map(y => if (y>pos) y-1 else y )))) <+
          // other case: is it a map?
          applyNTimes(pos-1)(stepDown)(isFullyAppliedMap) `;`
          basic.fail()
        )(e)
      case Nil => id[Rise]()(e)
      case _ => Failure(reorderRec(l))
    }

    def freduce(s: Strategy[Rise]) = function(function(argumentOf(ReduceSeq()(), body(body(s)))))
    def freduceX(s: Strategy[Rise]) = argument(function(function(argumentOf(ReduceSeq()(), body(body(s))))))
    def stepDown(s: Strategy[Rise]): Strategy[Rise] =
      freduceX(s) <+ freduce(s) <+ fmap(s)
  }
}
