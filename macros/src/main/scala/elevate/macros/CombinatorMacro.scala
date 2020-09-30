package elevate.macros

import scala.annotation.{StaticAnnotation, compileTimeOnly}
import scala.reflect.macros.blackbox
import scala.language.experimental.macros

// scalastyle:off indentation
object CombinatorMacro {
  val verbose = false

  // noinspection ScalaUnusedSymbol
  @compileTimeOnly("rule macro")
  class combinator(doc: String = "") extends StaticAnnotation {
    def macroTransform(annottees: Any*): Any = macro Impl.rule
  }

  // noinspection ScalaUnusedSymbol
  class Impl(val c: blackbox.Context) {
    import c.universe._

    def rule(annottees: c.Expr[Any]*): c.Expr[Any] = {
      annottees.map(_.tree) match {
        case (cdef: DefDef) :: Nil =>
          c.Expr(fromDefDef(cdef))
        case (cdef: DefDef) :: (md: ModuleDef) :: Nil =>
          c.Expr(q"{${fromDefDef(cdef)}; $md}")
        case _ => c.abort(c.enclosingPosition, "expected a method definition")
      }
    }

    def fromDefDef: DefDef => Tree = {
      case q"def $name[..$tparams]: Strategy[..$t1] => Strategy[..$t2] = (..$s) => $body"
        if s.length == 1 && t1.length == 1 && t2.length == 1 =>

        val sdef: ValDef = {
          val q"$m val $n = $rhs" = s.head
          ValDef(m, n, tq"Strategy[..$t1]", rhs)
        }

        makeCombinatorClass(name.asInstanceOf[TermName], tparams.asInstanceOf[List[TypeDef]],
          t2.head, List(List(sdef)), List(List()), List(List(sdef)), List(List()), body)

      case q"def $name[..$tparams]: Strategy[..$t1] => Strategy[..$t2] => Strategy[..$t3] = (..$s1) => (..$s2) => $body"
        if s1.length == 1 && s2.length == 1 && t1.length == 1 && t2.length == 1 && t3.length == 1 =>

        val s1def: ValDef = {
          val q"$m val $n = $rhs" = s1.head
          ValDef(m, n, tq"Strategy[..$t1]", rhs)
        }
        val s2def: ValDef = {
          val q"$m val $n = $rhs" = s2.head
          ValDef(m, n, tq"Strategy[..$t2]", rhs)
        }

        makeCombinatorClass(name.asInstanceOf[TermName], tparams.asInstanceOf[List[TypeDef]], t3.head,
          List(List(s1def, s2def)), List(List()), List(List(s1def), List(s2def)), List(List()), body)

      case q"def $name[..$tparams]: Strategy[..$t1] => Strategy[..$t2] => Strategy[..$t3] => Strategy[..$t4] = (..$s1) => (..$s2) => (..$s3) => $body"
        if s1.length == 1 && s2.length == 1 && s3.length == 1 &&
           t1.length == 1 && t2.length == 1 && t3.length == 1 && t4.length == 1 =>

        val s1def: ValDef = {
          val q"$m val $n = $rhs" = s1.head
          ValDef(m, n, tq"Strategy[..$t1]", rhs)
        }
        val s2def: ValDef = {
          val q"$m val $n = $rhs" = s2.head
          ValDef(m, n, tq"Strategy[..$t2]", rhs)
        }
        val s3def: ValDef = {
          val q"$m val $n = $rhs" = s3.head
          ValDef(m, n, tq"Strategy[..$t3]", rhs)
        }

        makeCombinatorClass(name.asInstanceOf[TermName], tparams.asInstanceOf[List[TypeDef]], t4.head,
          List(List(s1def, s2def, s3def)), List(List()), List(List(s1def), List(s2def), List(s3def)), List(List()), body)

      case q"def $name[..$tparams](...$paramLists): Strategy[..$t1] => Strategy[..$t2] = (..$s) => $body"
        if s.length == 1 && t1.length == 1 && t2.length == 1 =>
        // make new parameter list
        val sdef: ValDef = {
          val q"$m val $n = $rhs" = s.head
          ValDef(m, n, tq"Strategy[..$t1]", rhs)
        }
        // split regular and implicit parameters
        val (regularParamLists, implicitParamLists) =
          paramLists.asInstanceOf[List[List[ValDef]]].span { params =>
            params.nonEmpty &&
            !params.head.mods.hasFlag(c.universe.Flag.IMPLICIT)
          }
        // append new parameter list to the regular, but before the implicit once
        val extendedClassParams =
          (List(regularParamLists.flatten :+ sdef) ++ implicitParamLists).filterNot(_.isEmpty)

        val regularNonEmptyParamLists = regularParamLists.filterNot(_.isEmpty)

        makeCombinatorClass(name.asInstanceOf[TermName], tparams.asInstanceOf[List[TypeDef]], t2.head,
          extendedClassParams, regularNonEmptyParamLists, List(List(sdef)), implicitParamLists, body)

      case q"def $name[..$tparams](...$paramLists): Strategy[..$t1] => Strategy[..$t2] => Strategy[..$t3] = (..$s1) => (..$s2) => $body"
        if s1.length == 1 && s2.length == 1 && t1.length == 1 && t2.length == 1 && t3.length == 1 =>
        // make new parameter lists
        val s1def: ValDef = {
          val q"$m val $n = $rhs" = s1.head
          ValDef(m, n, tq"Strategy[..$t1]", rhs)
        }
        val s2def: ValDef = {
          val q"$m val $n = $rhs" = s2.head
          ValDef(m, n, tq"Strategy[..$t2]", rhs)
        }
        // split regular and implicit parameters
        val (regularParamLists, implicitParamLists) =
          paramLists.asInstanceOf[List[List[ValDef]]].span { params =>
            params.nonEmpty &&
            !params.head.mods.hasFlag(c.universe.Flag.IMPLICIT)
          }
        // append new parameter list to the regular, but before the implicit once
        val extendedClassParams =
          (regularParamLists ++ List(List(s1def), List(s2def)) ++ implicitParamLists).filterNot(_.isEmpty)

        val regularNonEmptyParamLists = regularParamLists.filterNot(_.isEmpty)

        makeCombinatorClass(name.asInstanceOf[TermName], tparams.asInstanceOf[List[TypeDef]],
          t3.head, extendedClassParams,
          regularNonEmptyParamLists, List(List(s1def), List(s2def)), implicitParamLists, body)

      case q"def $name[..$tparams](...$paramLists): Strategy[..$t1] => Strategy[..$t2] => Strategy[..$t3] => Strategy[..$t4] = (..$s1) => (..$s2) => (..$s3) => $body"
        if s1.length == 1 && s2.length == 1 && s3.length == 1 &&
           t1.length == 1 && t2.length == 1 && t3.length == 1 && t4.length == 1 =>
        // make new parameter lists
        val s1def: ValDef = {
          val q"$m val $n = $rhs" = s1.head
          ValDef(m, n, tq"Strategy[..$t1]", rhs)
        }
        val s2def: ValDef = {
          val q"$m val $n = $rhs" = s2.head
          ValDef(m, n, tq"Strategy[..$t2]", rhs)
        }
        val s3def: ValDef = {
          val q"$m val $n = $rhs" = s3.head
          ValDef(m, n, tq"Strategy[..$t3]", rhs)
        }
        // split regular and implicit parameters
        val (regularParamLists, implicitParamLists) =
          paramLists.asInstanceOf[List[List[ValDef]]].span { params =>
            params.nonEmpty &&
            !params.head.mods.hasFlag(c.universe.Flag.IMPLICIT)
          }
        // append new parameter list to the regular, but before the implicit once
        val extendedClassParams =
          (regularParamLists ++ List(List(s1def), List(s2def), List(s3def)) ++ implicitParamLists).filterNot(_.isEmpty)

        val regularNonEmptyParamLists = regularParamLists.filterNot(_.isEmpty)

        makeCombinatorClass(name.asInstanceOf[TermName], tparams.asInstanceOf[List[TypeDef]],
          t4.head, extendedClassParams, regularNonEmptyParamLists,
          List(List(s1def), List(s2def), List(s3def)), implicitParamLists, body)

      case _ =>
        c.abort(c.enclosingPosition, "expected a valid combinator:\n" +
          "1. def combinator: Strategy[P] => Strategy[P] = s => body\n" +
          "2. def combinator: Strategy[P] => Strategy[P] => Strategy[P] = fs => ss => body\n" +
          "3. def combinator: Strategy[P] => Strategy[P] => Strategy[P] => Strategy[P] = fs => ss => ts => body\n" +
          "4. def combinator(params): Strategy[P] => Strategy[P] = s => body\n" +
          "5. def combinator(params): Strategy[P] => Strategy[P] => Strategy[P] = fs => ss => body\n" +
          "6. def combinator(params): Strategy[P] => Strategy[P] => Strategy[P] => Strategy[P] = fs => ss => ts => body\n")
    }

    def makeCombinatorClass(name: TermName, tparams: List[TypeDef], t: Tree,
                            classParamLists: List[List[ValDef]],
                            regularParamLists: List[List[ValDef]],
                            funParamLists: List[List[ValDef]],
                            funImplParamLists: List[List[ValDef]],
                            body: Tree): Tree = {
      val className = if (name.toString.charAt(0).isLetterOrDigit) {
        name.toString.capitalize
      } else {
        name.toString + "_class"
      }
      val code = q"""
        final case class ${TypeName(className)}[..$tparams](...$classParamLists) extends Strategy[$t] {
          ..${makeApply(t, body)}

          ..${makeToString(name, classParamLists)}
        }

        ..${makeCompanionFunction(name, className, tparams, t, classParamLists,
                                  regularParamLists, funParamLists, funImplParamLists)}
        """
      if (verbose) {
        c.info(c.enclosingPosition,
          s"generated `${name.toString}'\n$code", force = false)
      }
      code
    }

    def makeApply(t: Tree, body: Tree): Tree = {
      body match {
        case q"(..$e) => $body" if e.length == 1 =>
          q"""
          override def apply(e_internal: $t): elevate.core.RewriteResult[$t] =
                ((${e.head.asInstanceOf[ValDef].name} : $t) => {
                  val res_internal: elevate.core.RewriteResult[$t] = $body
                  res_internal
                }).apply(e_internal)
          """
        case _ =>
          q"""
          override def apply(e_internal: $t): elevate.core.RewriteResult[$t] =
                ($body).apply(e_internal)
          """
      }
    }

    def makeToString(name: TermName, paramLists: List[List[ValDef]] = List()): Tree = {
      c.prefix.tree match {
        case q"new combinator($docTree)" =>
          q"override def toString: String = ${c.eval[String](c.Expr(docTree))}"
        case q"new combinator(doc = $docTree)" =>
          q"override def toString: String = ${c.eval[String](c.Expr(docTree))}"
        case _ =>
          if (paramLists.isEmpty) {
            q"override def toString: String = ${name.toString}"
          } else {
            q"""
        override def toString: String = ${name.toString} + ${
              paramLists.map { params =>
                q"""${params.map {
                  case ValDef(_, name, _, _) => q"$name.toString"
                }}.mkString("(", ",", ")")"""
              }
            }.mkString("(", ",", ")")
         """
          }
      }
    }

    def makeCompanionFunction(name: TermName,
                              className:String,
                              tparams: List[TypeDef],
                              t: Tree,
                              classParamLists: List[List[ValDef]],
                              regularParamLists: List[List[ValDef]],
                              funParamLists: List[List[ValDef]],
                              funImplParamLists: List[List[ValDef]]): Tree = {
      val argLists: List[List[Tree]] = classParamLists.map{ params => params.map {
        case ValDef(_, name, _, _) => q"$name"
      }}
      val targs = tparams.map{ case TypeDef(_, name, _, _) => tq"$name" }

      if (classParamLists.isEmpty) {
        q"""def $name[..$tparams]: Strategy[$t] =
           new ${TypeName(className)}[..$targs](...$argLists)"""
      } else {
        val returnType = funParamLists.map(p => p.map {
          case ValDef(_, _, tpt, _) => tpt
        }).foldRight(tq"Strategy[$t]")((a, b) =>
          a match {
            case List(tpt) => tq"$tpt => $b"
            case ts => q"(..$ts) => $b"
          }
        )

        val initBody: Tree = q"new ${TypeName(className)}[..$targs](...$argLists)"

        val body: Tree = funParamLists.foldRight(initBody)((a, b) =>
          a match {
            case List(p) => q"($p) => $b"
            case ps => q"(..$ps) => $b"
          }
        )
        if (regularParamLists.isEmpty || regularParamLists.head.isEmpty) {
          if (funImplParamLists.isEmpty || funImplParamLists.head.isEmpty) {
            q"def $name[..$tparams]: $returnType = $body"
          } else {
            q"def $name[..$tparams](...$funImplParamLists): $returnType = $body"
          }
        } else {
          if (funImplParamLists.isEmpty || funImplParamLists.head.isEmpty) {
            q"def $name[..$tparams](...$regularParamLists): $returnType = $body"
          } else {
            q"def $name[..$tparams](...$regularParamLists)(...$funImplParamLists): $returnType = $body"
          }
        }
      }
    }

  }

}
// scalastyle:on indentation
