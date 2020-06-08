package elevate.macros

import scala.annotation.{StaticAnnotation, compileTimeOnly}
import scala.reflect.macros.blackbox
import scala.language.experimental.macros

// scalastyle:off indentation
object RuleMacro {

  // noinspection ScalaUnusedSymbol
  @compileTimeOnly("rule macro")
  class rule extends StaticAnnotation {
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
      case q"def $name: Strategy[..$targs] = { case ..$cases }" if targs.length == 1 =>
        makeRuleObject(name.asInstanceOf[TermName], targs.head, cases.asInstanceOf[List[CaseDef]])
      case q"def $name: Strategy[..$targs] = (..$e) => $body" if targs.length == 1 && e.length == 1 =>
        makeRuleObject(name.asInstanceOf[TermName], targs.head, e.head.asInstanceOf[ValDef], body)

      case q"def $name(...$params): Strategy[..$targs] = { case ..$cases }"
        if targs.length == 1 && params.length == 1 =>
          makeRuleClass(name.asInstanceOf[TermName],
            targs.head,
            params.head.asInstanceOf[List[ValDef]],
            cases.asInstanceOf[List[CaseDef]])
      case q"def $name(...$params): Strategy[..$targs] = (..$e) => $body"
        if targs.length == 1 && params.length == 1 && e.length == 1 =>
          makeRuleClass(name.asInstanceOf[TermName],
            targs.head,
            params.head.asInstanceOf[List[ValDef]],
            e.head.asInstanceOf[ValDef],
            body)

      case _ =>
        c.abort(c.enclosingPosition, "expected a valid rule definition:\n" +
          "1. def rule: Strategy[P] = { case pattern => replacement }\n" +
          "2. def rule: Strategy[P] = e => body\n" +
          "3. def rule(params): Strategy[P] = { case pattern => replacement }\n" +
          "4. def rule(params): Strategy[P] = e => body\n")
    }

    def makeRuleObject(name: TermName, tparam: Tree, cases: List[CaseDef]): Tree = {
      val c = q"""final case object $name extends Strategy[$tparam] {
        override def apply(e_internal: $tparam): RewriteResult[$tparam] = e_internal match {
          case ..${makeCases(cases, q"Failure($name)")}
        }

         override def toString: String = ${name.toString}
      }"""
//      println(c)
      c
    }

    def makeRuleObject(name: TermName, tparam: Tree, e: ValDef, body: Tree): Tree = {
      val c = q"""final case object $name extends Strategy[$tparam] {
        override def apply(e_internal: $tparam): RewriteResult[$tparam] =
            ((${e.name} : $tparam) => {
              val res_internal: RewriteResult[$tparam] = $body
              res_internal
            }).apply(e_internal)

         override def toString: String = ${name.toString}
      }"""
//      println(c)
      c
    }

    def makeRuleClass(name: TermName, tparam: Tree, params: List[ValDef],
                      cases: List[CaseDef]): Tree = {
      val c = q"""
        final case class ${TypeName(name.toString)}(..$params) extends Strategy[$tparam] {
          override def apply(e_internal: $tparam): RewriteResult[$tparam] = e_internal match {
            case ..${makeCases(cases, q"Failure($name(..${params.map{
              case ValDef(_, name, _, _) => q"$name"
              }}))")}
          }

          override def toString: String = ${name.toString} + ${params.map{
              case ValDef(_, name, _, _) => q"$name.toString"
            }}.mkString("(", ",", ")")
        }"""
//      println(c)
      c
    }

    def makeRuleClass(name: TermName, tparam: Tree, params: List[ValDef],
                      e: ValDef, body: Tree): Tree = {
      val c = q"""
        final case class ${TypeName(name.toString)}(..$params) extends Strategy[$tparam] {
          override def apply(e_internal: $tparam): RewriteResult[$tparam] =
            ((${e.name} : $tparam) => {
              val res_internal: RewriteResult[$tparam] = $body
              res_internal
            }).apply(e_internal)

          override def toString: String = ${name.toString} + ${params.map{
        case ValDef(_, name, _, _) => q"$name.toString"
      }}.mkString("(", ",", ")")
        }"""
//      println(c)
      c
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
  }
}
// scalastyle:on indentation
