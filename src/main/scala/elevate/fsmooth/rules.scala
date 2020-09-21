package elevate.fsmooth

import elevate.core._
import FSmooth.VectorFunctionConstants.build
import FSmooth._
import FSmooth.DSL._
import FSmooth.ScalarFunctionConstants._
import FSmooth.VectorFunctionConstants._
import FSmooth.PairFunctionConstants._
import FSmooth.ValueConstants._
import elevate.macros.RuleMacro.rule

/* Implementing rules for the F~ language as described in:

@article{DBLP:journals/pacmpl/ShaikhhaFVJ19,
  author    = {Amir Shaikhha and
               Andrew Fitzgibbon and
               Dimitrios Vytiniotis and
               Simon {Peyton Jones}},
  title     = {Efficient differentiable programming in a functional array-processing
               language},
  journal   = {{PACMPL}},
  volume    = {3},
  number    = {{ICFP}},
  pages     = {97:1--97:30},
  year      = {2019}
}

 */
object rules {

  // lambda calculus rules

  @rule("(fun x -> e_0) e_1 ~> let x = e_1 in e_0")
  def funToLet: Strategy[FSmooth] = {
    case Application(Abstraction(Seq(x), e0, _), Seq(e1), _) => Success(Let(x, e1, e0))
    case Application(Abstraction(Seq(x,y), e0, _), Seq(e1, e2), _) => Success(Let(x, e1, Let(y, e2, e0)))
  }

  @rule("let x = e_0 in e_1 ~> e_1[x / e_0]")
  def letPartialEvaluation: Strategy[FSmooth] = {
    case Let(x, e0, e1, _) => Success(replace(x).`with`(e0).in(e1))
  }

  @rule("let x = let y = e_0 in e_1 ~> let y = e_0 in let x = e_1 in e_2")
  def letFission: Strategy[FSmooth] = {
    case Let(x, Let(y, e0, e1, _), e2, _) => Success(Let(x, e0, Let(y, x, e1)))
  }

  @rule("let x = e_0 in let y = e_0 in e_1 ~> let x = e_0 in let y = x in e_1")
  def letInitDuplication: Strategy[FSmooth] = {
    case Let(x, e00, Let(y, e01, e1, _), _) if e00 == e01 => Success(Let(x, e00, Let(y, x, e1)))
  }

  @rule("let x = e_0 in let y = e_1 in e_2 ~> let y = e_1 in let x = e_0 in e_2")
  def letSwap: Strategy[FSmooth] = {
    case Let(x, e0, Let(y, e1, e2, _), _) => Success(Let(y, e1, Let(x, e0, e2)))
  }

  @rule("f(let x = e_0 in e_1 ~> let x = e_0 in f(e_1)")
  def letApplication: Strategy[FSmooth] = {
    case Application(f, Seq(Let(x, e0, e1, _)), _) => Success(Let(x, e0, Application(f, Seq(e1))))
  }

  // ring-structure rules

  @rule("e + 0 = 0 + e ~> e")
  def additionZero: Strategy[FSmooth] = {
    case Application(`+`(_), Seq(e, ScalarValue(0)), _) => Success(e)
    case Application(`+`(_), Seq(ScalarValue(0), e), _) => Success(e)
  }

  @rule("e * 1 = 1 * e ~> e")
  def multiplicationOne: Strategy[FSmooth] = {
    case Application(`*`(_), Seq(e, ScalarValue(1)), _) => Success(e)
    case Application(`*`(_), Seq(ScalarValue(1), e), _) => Success(e)
  }

  @rule("e * 0 = 0 * e ~> 0")
  def multiplicationZero: Strategy[FSmooth] = {
    case Application(`*`(_), Seq(e, ScalarValue(0)), _) => Success(ScalarValue(0))
    case Application(`*`(_), Seq(ScalarValue(0), e), _) => Success(ScalarValue(0))
  }

  @rule("e + -e = e - e ~> 0")
  def additionSimplification: Strategy[FSmooth] = {
    case Application(`+`(_), Seq(e1,
         Application(`-`(_), Seq(e2), _)), _) if e1 == e2 => Success(ScalarValue(0))
    case Application(`-`(_), Seq(e1, e2), _)  if e1 == e2 => Success(ScalarValue(0))
  }

  @rule("e_0 * e_1 + e_0 * e_2 ~> e_0 * (e_1 + e_2)")
  def factorization: Strategy[FSmooth] = {
    case Application(`+`(_), Seq(
         Application(`*`(_), Seq(e01, e1), _),
         Application(`*`(_), Seq(e02, e2), _)), _) if e01 == e02 =>
      Success(Application(`*`(freshTypeVar), Seq(e01,
              Application(ScalarFunctionConstants.`+`(freshTypeVar), Seq(e1, e2)))))
  }

  // conditional rules

  @rule("if true then e_1 else e_2 ~> e_1")
  def conditionTrue: Strategy[FSmooth] = {
    case Conditional(`true`, e1, _, _) => Success(e1)
  }

  @rule("if false then e_1 else e_2 ~> e_2")
  def conditionFalse: Strategy[FSmooth] = {
    case Conditional(`false`, _, e2, _) => Success(e2)
  }

  @rule("if e_0 then e_1 else e_1 ~> e_1")
  def uselessConditional: Strategy[FSmooth] = {
    case Conditional(_, e0, e1, _) if e0 == e1 => Success(e1)
  }

  @rule("if e_0 then e_1 else e_2 ~> if e_0 then e_1[e_0 / true] else e_2[e_0 / false]")
  def conditionalPartialEvalution: Strategy[FSmooth] = {
    case Conditional(e0, e1, e2, _) => Success(Conditional(e0,
                                          replace(e0).`with`(`true`).in(e1),
                                          replace(e0).`with`(`false`).in(e2)))
  }

  @rule("f(if e_0 then e_1 else e_2) ~> if e_0 then f(e_1) else f(e_2)")
  def conditionApplication: Strategy[FSmooth] = {
    case Application(f, Seq(Conditional(e0, e1, e2, _)), _) =>
      Success(Conditional(e0, Application(f, Seq(e1)), Application(f, Seq(e2))))
    case Application(f, Seq(arg0, Conditional(e0, e1, e2, _)), _) =>
      Success(Conditional(e0, Application(f, Seq(arg0, e1)), Application(f, Seq(arg0, e2))))
  }

  // loop fusion rules

  @rule("(build e_0 e_1)[e_2] ~> e_1 e_2")
  def buildGet: Strategy[FSmooth] = {
    case Application(`get`(_), Seq(Application(`build`(_), Seq(_,e1), _), e2), _) => Success(Application(e1, Seq(e2)))
  }

  @rule("length(build e_0 e_1) ~> e_0")
  def lengthBuild: Strategy[FSmooth] = {
    case Application(`length`(_), Seq(Application(`build`(_), Seq(e0, _), _)), _) => Success(e0)
  }

  // loop normalisation rules
  @rule("ifold f z 0 ~> z")
  def trivialFold: Strategy[FSmooth] = {
    case Application(`ifold`(_), Seq(_, z, ScalarValue(0)), _) => Success(z)
  }

  @rule("ifold f z n ~> ifold(fun a i -> f a (i+1))(f z 0)(n -1)")
  def foldInsertFun: Strategy[FSmooth] = {
    case Application(`ifold`(_), Seq(f, z, n), _) =>
      val (a, i) = (Variable("a"), Variable("i"))
      Success(Application(VectorFunctionConstants.`ifold`(freshTypeVar), Seq(
        Abstraction(Seq(a, i),
          Application(f, Seq(a, Application(ScalarFunctionConstants.`+`(freshTypeVar), Seq(i, ScalarValue(1)))))),
        Application(f, Seq(z, ScalarValue(0))),
        Application(ScalarFunctionConstants.`-`(freshTypeVar), Seq(n, ScalarValue(1)))
      )))
  }

  @rule("ifold(fun a i -> a) z n ~> z")
  def foldSimplification: Strategy[FSmooth] = {
    case Application(`ifold`(_), Seq(Abstraction(Seq(a1, _), a2, _), z, _), _) if a1 == a2 => Success(z)
  }

  @rule("ifold(fun a i -> if (i = e_0) then e_1 else a) z n ~> e_1 (if e_0 does not mention a or i)")
  def foldConditional: Strategy[FSmooth] = {
    case Application(`ifold`(_), Seq(
      Abstraction(Seq(a1, i1), Conditional(Application(`=:=`(_), Seq(i2, e0), _), e1, a2, _), _),
      z,
      n), _) =>
      //if a1 == a2 && i1 == i2 && !contains[FSmooth](a1).apply(e0) && !contains[FSmooth](i1).apply(e0) =>
      Success(Let(a1, z, Let(i1, e0, e1)))
  }

  // tuple normalisation rules

  @rule("fst (e_0, e_1) ~> e_0")
  def pairFst: Strategy[FSmooth] = {
    case Application(`fst`(_), Seq(Application(`pair`(_), Seq(e0, _), _)), _) => Success(e0)
  }

  @rule("snd(e_0, e_1) ~> e_1")
  def pairSnd: Strategy[FSmooth] = {
    case Application(`snd`(_), Seq(Application(`pair`(_), Seq(_, e1), _)), _) => Success(e1)
  }

  // loop fission rule

  @rule
  def loopFission: Strategy[FSmooth] = {
    case Application(`ifold`(_), Seq(
         Abstraction(Seq(a1,i1), Application(`pair`(_), Seq(
            Application(f0, Seq(Application(`fst`(_), Seq(a2), _), i2), _),
            Application(f1, Seq(Application(`snd`(_), Seq(a3), _), i3), _)), _), _),
         Application(`pair`(_), Seq(z0, z1), _), n), _) if a1 == a2 && a1 == a3 && i1 == i2 && i1 == i2 =>

      Success(Application(PairFunctionConstants.`pair`(freshTypeVar), Seq(
        Application(VectorFunctionConstants.`ifold`(freshTypeVar), Seq(f0, z0, n)),
        Application(VectorFunctionConstants.`ifold`(freshTypeVar), Seq(f1, z1, n))
      )))
  }
}
