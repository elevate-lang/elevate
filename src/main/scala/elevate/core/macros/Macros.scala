package elevate.core.macros

import elevate.core.RewriteResult._
import elevate.core.{RewriteResult, Strategy}

import scala.quoted._

inline def strategy[P](inline name: String,
                       inline rule: PartialFunction[P, RewriteResult[P]]): Strategy[P] =
  ${ strategyMacro('name, 'rule) }

inline def strategy[P](inline name: String,
                       inline rule: Strategy[P]): Strategy[P] =
  ${ strategyMacro('name, 'rule) }

inline def rule[P](inline name: String,
                   inline rule: Strategy[P]): Strategy[P] =
  ${ strategyMacro('name, 'rule) }
  
inline def rule[P](inline name: String,
                   inline rule: PartialFunction[P, RewriteResult[P]]): Strategy[P] =
  ${ strategyMacro('name, 'rule) }

inline def combinator[P](inline name: String,
                         inline f: Strategy[P] => Strategy[P] => Strategy[P]
                        ): Strategy[P] => Strategy[P] => Strategy[P] =
  ${ combinatorMacro('name, 'f) }

def strategyMacro[P](name: Expr[String],
                     s: Expr[Strategy[P]])
                    (using Type[P])
                    (using Quotes): Expr[Strategy[P]] =
  val expr = '{
    new Strategy[P] {
      override def apply(p: P): RewriteResult[P] =
        try
          $s(p)
        catch
          case _: MatchError => Failure(this)
          
      override def toString(): String = $name
    }
  }
  println(name.show + "\n" + expr.show)
  expr

def combinatorMacro[P](name: Expr[String],
                       f: Expr[Strategy[P] => Strategy[P] => Strategy[P]])
                      (using Type[P])
                      (using Quotes): Expr[Strategy[P] => Strategy[P] => Strategy[P]] =
  val expr = '{
    (fs: Strategy[P]) => (ss: Strategy[P]) => new Strategy[P] {
      override def apply(p: P): RewriteResult[P] =
        try
          $f(fs)(ss)(p)
        catch
          case _: MatchError => Failure(this)
          
      override def toString(): String = $name + s"(${fs.toString})(${ss.toString})"
    }
  }
  println(name.show + "\n" + expr.show)
  expr
