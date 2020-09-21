package elevate.core.strategies

import elevate.core.{RewriteResult, Strategy, Success}
import elevate.macros.RuleMacro.rule

object debug {

  @rule("peek(f)") def peek[P](f: P => Unit): Strategy[P] =
    p => { f(p); Success(p) }

  @rule def debug[P](msg: String): Strategy[P] =
    peek[P](x => println(msg + "\n" + x))

  @rule def echo[P](msg: String): Strategy[P] =
    peek[P](_ => println(msg))
}
