package FSmooth

import FSmooth.DSL.freshTypeVar

export Expr.{Cases => _, _}

sealed trait Expr(val t: Type)
object Expr:
  export Cases._
  
  enum Cases(override val t: Type) extends Expr(t):
    case Variable(name: String,
                  override val t: Type = freshTypeVar) extends Cases(t)
    case Abstraction(params: Seq[Cases.Variable],
                     body: Expr,
                     override val t: Type = freshTypeVar) extends Cases(t)
    case Application(fun: Expr,
                     args: Seq[Expr],
                     override val t: Type = freshTypeVar) extends Cases(t)
    case Let(x: Cases.Variable,
             value: Expr,
             body: Expr,
             override val t: Type = freshTypeVar) extends Cases(t)
    case Conditional(cond: Expr,
                     thenBranch: Expr,
                     elseBranch: Expr,
                     override val t: Type = freshTypeVar) extends Cases(t)
    case ScalarValue(n: Double) extends Cases(Double)
    case IndexValue(i: Int) extends Cases(Index)
    case CardinalityValue(N: Int) extends Cases(Card)
  end Cases

  abstract class Constants(override val t: Type) extends Expr(t):
    def typeScheme: Type
    def setType(t: Type): Constants
end Expr

extension (e: Expr)
  def setType(t: Type): Expr = e match
    case v: Expr.Variable => v.copy(t = t)
    case a: Expr.Abstraction => a.copy(t = t)
    case a: Expr.Application => a.copy(t = t)
    case l: Expr.Let => l.copy(t = t)
    case c: Expr.Conditional => c.copy(t = t)
    case Expr.ScalarValue(_) => assert(t == Double); e
    case Expr.IndexValue(_) => assert(t == Index); e
    case Expr.CardinalityValue(_) => assert(t == Card); e
    case c: Expr.Constants => c.setType(t)
