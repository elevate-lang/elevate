package elevate.macros

import scala.annotation.{StaticAnnotation, compileTimeOnly}
import scala.reflect.macros.blackbox
import scala.language.experimental.macros

// scalastyle:off indentation
object RuleMacro {
  val verbose = false

  // noinspection ScalaUnusedSymbol
  @compileTimeOnly("rule macro")
  class rule(doc: String = "") extends StaticAnnotation {
    def macroTransform(annottees: Any*): Any = macro Impl.rule
  }

  // noinspection ScalaUnusedSymbol
  class Impl(val c: blackbox.Context) {
    import c.universe._

    def strategy(annottees: c.Expr[Any]*): c.Expr[Any] =
      generic({ r: Tree => r }, annottees)

    def rule(annottees: c.Expr[Any]*): c.Expr[Any] =
      generic({ r: Tree => q"elevate.core.countApplications { $r }" }, annottees)

    def generic(wrapResult: Tree => Tree, annottees: Seq[c.Expr[Any]]): c.Expr[Any] = {
      annottees.map(_.tree) match {
        case (cdef: DefDef) :: Nil =>
          c.Expr(fromDefDef(wrapResult)(cdef))
        case (cdef: DefDef) :: (md: ModuleDef) :: Nil =>
          c.Expr(q"{${fromDefDef(wrapResult)(cdef)}; $md}")
        case _ => c.abort(c.enclosingPosition, "expected a method definition")
      }
    }

    def fromDefDef(wrapResult: Tree => Tree): DefDef => Tree = {
      case q"def ${name: TermName}[..$tparams]: Strategy[..$resTypes] = { case ..$cases }"
        if resTypes.length == 1 =>

        val resType = resTypes.head.asInstanceOf[Tree]
        if (tparams.isEmpty) {
          val r = q"""e_internal match {
            case ..${addDefaultCase(cases.asInstanceOf[List[CaseDef]], q"elevate.core.Failure($name)")}
          }"""
          makeRuleObject(name, resType,
            q"""
            override def apply(e_internal: $resType): elevate.core.RewriteResult[$resType] = ${wrapResult(r)}"""
          )
        } else {
          makeRuleClass(name, tparams.asInstanceOf[List[TypeDef]], resType, List(List())) { makeClass =>
            val r = q"""e_internal match {
              case ..${addDefaultCase(cases.asInstanceOf[List[CaseDef]], q"elevate.core.Failure($makeClass)")}
            }"""
            q"""
            override def apply(e_internal: $resType): elevate.core.RewriteResult[$resType] = ${wrapResult(r)}

            ..${makeToString(name)}
            """
          }
        }

      case q"def ${name: TermName}[..$tparams]: Strategy[..$resTypes] = (..$es) => $body"
        if resTypes.length == 1 && es.length == 1 =>

        val resType = resTypes.head.asInstanceOf[Tree]
        val e = es.head.asInstanceOf[ValDef]
        if (tparams.isEmpty) {
          val r = q"""((${e.name} : $resType) => {
            val res_internal: elevate.core.RewriteResult[$resType] = $body
            res_internal
          }).apply(e_internal)"""
          makeRuleObject(name, resType,
            q"""
            override def apply(e_internal: $resType): elevate.core.RewriteResult[$resType] = ${wrapResult(r)}
            """)
        } else {
          val r = q"""{
            ((${e.name} : $resType) => {
              val res_internal: elevate.core.RewriteResult[$resType] = $body
              res_internal
            }).apply(e_internal)
          }"""
          makeRuleClass(name, tparams.asInstanceOf[List[TypeDef]], resType, List(List()))(_ =>
            q"""
            override def apply(e_internal: $resType): elevate.core.RewriteResult[$resType] = ${wrapResult(r)}

            ..${makeToString(name)}
            """
          )
        }

      case q"def ${name: TermName}[..$tparams]: Strategy[..$resTypes] = $body"
        if resTypes.length == 1 =>

        val resType = resTypes.head.asInstanceOf[Tree]
        val r = q"""($body).apply(e_internal)"""
        if (tparams.isEmpty) {
          makeRuleObject(name, resType,
            q"""
            override def apply(e_internal: $resType): elevate.core.RewriteResult[$resType] = ${wrapResult(r)}
            """)
        } else {
          makeRuleClass(name, tparams.asInstanceOf[List[TypeDef]], resType, List(List()))(_ =>
            q"""
          override def apply(e_internal: $resType): elevate.core.RewriteResult[$resType] = ${wrapResult(r)}

          ..${makeToString(name)}
        """
          )
        }

        // def foo (3, 4, 5)(6, 7, 8)
        // paramLists = List( List(Tree, Tree, Tree), List(Tree, Tree, Tree) )
      case q"def ${name: TermName}[..$tparams](...$paramLists): Strategy[..$resTypes] = { case ..$cases }"
        if /*paramLists.length == 1 &&*/ resTypes.length == 1 =>

          val params = paramLists.asInstanceOf[List[List[ValDef]]]
          val resType = resTypes.head.asInstanceOf[Tree]
          makeRuleClass(name, tparams.asInstanceOf[List[TypeDef]], resType, params) { makeClass =>
            val r = q"""e_internal match {
              case ..${addDefaultCase(cases.asInstanceOf[List[CaseDef]], q"elevate.core.Failure($makeClass)")}
            }"""
            q"""
            override def apply(e_internal: $resType): elevate.core.RewriteResult[$resType] = ${wrapResult(r)}

            ..${makeToString(name, params)}
            """
          }

      case q"def ${name: TermName}[..$tparams](...$paramLists): Strategy[..$resTypes] = (..$es) => $body"
        if /*paramLists.length == 1 &&*/ resTypes.length == 1 && es.length == 1 =>

        val params = paramLists.asInstanceOf[List[List[ValDef]]]
        val resType = resTypes.head.asInstanceOf[Tree]
        val e = es.head.asInstanceOf[ValDef]
        val r = q"""
            ((${e.name} : $resType) => {
              val res_internal: elevate.core.RewriteResult[$resType] = $body
              res_internal
            }).apply(e_internal)"""
        makeRuleClass(name, tparams.asInstanceOf[List[TypeDef]], resType, params)(_ =>
          q"""
          override def apply(e_internal: $resType): elevate.core.RewriteResult[$resType] = ${wrapResult(r)}

          ..${makeToString(name, params)}
        """
        )


      case q"def ${name: TermName}[..$tparams](...$paramLists): Strategy[..$resTypes] = $body"
        if /*paramLists.length == 1 &&*/ resTypes.length == 1 =>

        val params = paramLists.asInstanceOf[List[List[ValDef]]]
        val resType = resTypes.head.asInstanceOf[Tree]
        val r = q"""($body).apply(e_internal)"""
        makeRuleClass(name, tparams.asInstanceOf[List[TypeDef]], resType, params)(_ =>
          q"""
          override def apply(e_internal: $resType): elevate.core.RewriteResult[$resType] = ${wrapResult(r)}

          ..${makeToString(name, params)}
        """
        )

      case _ =>
        c.abort(c.enclosingPosition, "expected a valid rule definition:\n" +
          "1. def rule: Strategy[P] = { case pattern => replacement }\n" +
          "2. def rule: Strategy[P] = e => body\n" +
          "3. def rule: Strategy[P] = body\n" +
          "4. def rule(params): Strategy[P] = { case pattern => replacement }\n" +
          "5. def rule(params): Strategy[P] = e => body\n" +
          "6. def rule(params): Strategy[P] = body\n")
    }

    def makeRuleObject(name: TermName, resType: Tree, apply: Tree): Tree = {
      val code = q"""final case object $name extends Strategy[$resType] {
        ..$apply

        ..${makeToString(name)}
      }"""
      if (verbose) {
        c.info(c.enclosingPosition,
          s"generated `${name.toString}'\n$code", force = false)
      }
      code
    }

    def makeRuleClass(name: TermName, tparams: List[TypeDef],
                      resType: Tree, params: List[List[ValDef]])
                     (body: Tree => Tree): Tree = {
      val className = if (name.toString.charAt(0).isLetterOrDigit && name.toString.charAt(0).isLower) {
        name.toString.capitalize
      } else {
        name.toString + "_class"
      }
      val makeClass = q"${TermName(className)}[..${tparams.map{
        case TypeDef(_, name, _, _) => tq"$name"
      }}](...${params.map(p => p.map {
        case ValDef(_, name, _, _) => q"$name"
      })})"

      val code = q"""
        final case class ${TypeName(className)}[..$tparams](...$params) extends Strategy[$resType] {
          ..${body(makeClass)}
        }

        ${if (params.size == 1 && params.head.isEmpty) {
            q"def $name[..$tparams]: Strategy[$resType] = $makeClass"
          } else {
            q"def $name[..$tparams](...$params): Strategy[$resType] = $makeClass"
        }}
        """
      if (verbose) {
        c.info(c.enclosingPosition,
          s"generated `${name.toString}'\n$code", force = false)
      }
      code
    }

    def addDefaultCase(cases: List[CaseDef], default: Tree): List[CaseDef] = {
      var defaultCaseMissing = true
      cases.foreach {
        case CaseDef(pq"_", _, _) => defaultCaseMissing = false
        case _ =>
      }
      if (cases.length == 1) {
        cases.head match {
          case cq"$_ @ $x => $_" => x match {
            case pq"_" => defaultCaseMissing = false
            case _ =>
          }
          case _ =>
        }
      }
      if (defaultCaseMissing) {
        cases :+ CaseDef(pq"_", EmptyTree, default)
      } else {
        cases
      }
    }

    def makeToString(name: TermName, params: List[List[ValDef]] = List(List())): Tree = {
      c.prefix.tree match {
        case q"new rule($docTree)" =>
          q"override def toString: String = ${c.eval[String](c.Expr(docTree))}"
        case q"new rule(doc = $docTree)" =>
          q"override def toString: String = ${c.eval[String](c.Expr(docTree))}"
        case _ =>
          if (params.head.isEmpty) {
            q"override def toString: String = ${name.toString}"
          } else {
            q"""
        override def toString: String = ${name.toString} + ${
              params.head.map{
                case ValDef(_, name, _, _) => q"$name.toString"
              }
            }.mkString("(", ",", ")")
         """
          }
      }
    }
  }
}
// scalastyle:on indentation
