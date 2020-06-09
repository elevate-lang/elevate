package elevate.macros

import scala.annotation.{StaticAnnotation, compileTimeOnly}
import scala.reflect.macros.blackbox
import scala.language.experimental.macros

// scalastyle:off indentation
object RuleMacro {

  // noinspection ScalaUnusedSymbol
  @compileTimeOnly("rule macro")
  class rule(doc: String = "") extends StaticAnnotation {
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
      case q"def $name[..$tparams]: Strategy[..$resType] = { case ..$cases }" if resType.length == 1 =>
        if (tparams.isEmpty) {
          makeRuleObject(name.asInstanceOf[TermName],
            resType.head, cases.asInstanceOf[List[CaseDef]])
        } else {
          makeRuleClass(name.asInstanceOf[TermName], tparams.asInstanceOf[List[TypeDef]],
            resType.head, List(), cases.asInstanceOf[List[CaseDef]])
        }
      case q"def $name[..$tparams]: Strategy[..$resType] = (..$e) => $body" if resType.length == 1 && e.length == 1 =>
        if (tparams.isEmpty) {
          makeRuleObject(name.asInstanceOf[TermName],
            resType.head, e.head.asInstanceOf[ValDef], body)
        } else {
          makeRuleClass(name.asInstanceOf[TermName], tparams.asInstanceOf[List[TypeDef]],
            resType.head, List(), e.head.asInstanceOf[ValDef], body)
        }
      case q"def $name[..$tparams]: Strategy[..$resType] = $body" if resType.length == 1 =>
        if (tparams.isEmpty) {
          makeRuleObject(name.asInstanceOf[TermName],
            resType.head, body)
        } else {
          makeRuleClass(name.asInstanceOf[TermName], tparams.asInstanceOf[List[TypeDef]],
            resType.head, List(), body)
        }

      case q"def $name[..$tparams](...$params): Strategy[..$resType] = { case ..$cases }"
        if resType.length == 1 && params.length == 1 =>
          makeRuleClass(name.asInstanceOf[TermName],
            tparams,
            resType.head,
            params.head.asInstanceOf[List[ValDef]],
            cases.asInstanceOf[List[CaseDef]])
      case q"def $name[..$tparams](...$params): Strategy[..$resType] = (..$e) => $body"
        if resType.length == 1 && params.length == 1 && e.length == 1 =>
          makeRuleClass(name.asInstanceOf[TermName],
            tparams,
            resType.head,
            params.head.asInstanceOf[List[ValDef]],
            e.head.asInstanceOf[ValDef],
            body)
      case q"def $name[..$tparams](...$params): Strategy[..$resType] = $body"
        if resType.length == 1 && params.length == 1 =>
        makeRuleClass(name.asInstanceOf[TermName],
          tparams,
          resType.head,
          params.head.asInstanceOf[List[ValDef]],
          body)

      case _ =>
        c.abort(c.enclosingPosition, "expected a valid rule definition:\n" +
          "1. def rule: Strategy[P] = { case pattern => replacement }\n" +
          "2. def rule: Strategy[P] = e => body\n" +
          "3. def rule: Strategy[P] = body\n" +
          "4. def rule(params): Strategy[P] = { case pattern => replacement }\n" +
          "5. def rule(params): Strategy[P] = e => body\n" +
          "6. def rule(params): Strategy[P] = body\n")
    }

    def makeRuleObject(name: TermName, resType: Tree, cases: List[CaseDef]): Tree = {
      val c = q"""final case object $name extends Strategy[$resType] {
        override def apply(e_internal: $resType): elevate.core.RewriteResult[$resType] = e_internal match {
          case ..${makeCases(cases, q"elevate.core.Failure($name)")}
        }

         ..${makeToString(name)}
      }"""
//      println(c)
      c
    }

    def makeRuleObject(name: TermName, resType: Tree, e: ValDef, body: Tree): Tree = {
      val c = q"""final case object $name extends Strategy[$resType] {
        override def apply(e_internal: $resType): elevate.core.RewriteResult[$resType] = {
            ((${e.name} : $resType) => {
              val res_internal: elevate.core.RewriteResult[$resType] = $body
              res_internal
            }).apply(e_internal)
        }

        ..${makeToString(name)}
      }"""
//      println(c)
      c
    }

    def makeRuleObject(name: TermName, resType: Tree, body: Tree): Tree = {
      val c = q"""final case object $name extends Strategy[$resType] {
        override def apply(e_internal: $resType): elevate.core.RewriteResult[$resType] = {
            ($body).apply(e_internal)
        }

        ..${makeToString(name)}
      }"""
      //      println(c)
      c
    }

    def makeRuleClass(name: TermName, tparams: List[TypeDef],
                      resType: Tree, params: List[ValDef])
                     (body: Tree => Tree): Tree = {
      val className = if (name.toString.charAt(0).isLetterOrDigit) {
        name.toString.capitalize
      } else {
        name.toString + "_class"
      }
      val makeClass = q"${TermName(className)}[..${tparams.map{
        case TypeDef(_, name, _, _) => tq"$name"
      }}](..${params.map{
        case ValDef(_, name, _, _) => q"$name"
      }})"

      val c = q"""
        final case class ${TypeName(className)}[..$tparams](..$params) extends Strategy[$resType] {
          ..${body(makeClass)}
        }

        ${if (params.isEmpty) {
            q"def $name[..$tparams]: Strategy[$resType] = $makeClass"
          } else {
            q"def $name[..$tparams](..$params): Strategy[$resType] = $makeClass"
        }}
        """
//      println(c)
      c
    }

    def makeRuleClass(name: TermName, tparams: List[TypeDef],
                      resType: Tree, params: List[ValDef],
                      cases: List[CaseDef]): Tree = {
      makeRuleClass(name, tparams, resType, params)(makeClass =>
        q"""
          override def apply(e_internal: $resType): elevate.core.RewriteResult[$resType] = e_internal match {
            case ..${makeCases(cases, q"elevate.core.Failure($makeClass)")}
          }

          ..${makeToString(name, params)}
        """
      )
    }

    def makeRuleClass(name: TermName, tparams: List[TypeDef],
                      resType: Tree, params: List[ValDef],
                      e: ValDef, body: Tree): Tree = {
      makeRuleClass(name, tparams, resType, params)(_ =>
        q"""
          override def apply(e_internal: $resType): elevate.core.RewriteResult[$resType] =
            ((${e.name} : $resType) => {
              val res_internal: elevate.core.RewriteResult[$resType] = $body
              res_internal
            }).apply(e_internal)

          ..${makeToString(name, params)}
        """
      )
    }

    def makeRuleClass(name: TermName, tparams: List[TypeDef],
                      resType: Tree, params: List[ValDef],
                      body: Tree): Tree = {
      makeRuleClass(name, tparams, resType, params)(_ =>
        q"""
          override def apply(e_internal: $resType): elevate.core.RewriteResult[$resType] =
            ($body).apply(e_internal)

          ..${makeToString(name, params)}
        """
      )
    }

    def makeCases(cases: List[CaseDef], default: Tree): List[CaseDef] = {
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

    def makeToString(name: TermName, params: List[ValDef] = List()): Tree = {
      c.prefix.tree match {
        case q"new rule($docTree)" =>
          q"override def toString: String = ${c.eval[String](c.Expr(docTree))}"
        case q"new rule(doc = $docTree)" =>
          q"override def toString: String = ${c.eval[String](c.Expr(docTree))}"
        case _ =>
          if (params.isEmpty) {
            q"override def toString: String = ${name.toString}"
          } else {
            q"""
        override def toString: String = ${name.toString} + ${
              params.map {
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
