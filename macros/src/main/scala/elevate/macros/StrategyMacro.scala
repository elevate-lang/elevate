package elevate.macros

import elevate.macros.RuleMacro.Impl

import scala.annotation.{StaticAnnotation, compileTimeOnly}
import scala.language.experimental.macros

object StrategyMacro {

  // noinspection ScalaUnusedSymbol
  @compileTimeOnly("strategy macro")
  class strategy(doc: String = "") extends StaticAnnotation {
    def macroTransform(annottees: Any*): Any = macro Impl.strategy
  }
}
