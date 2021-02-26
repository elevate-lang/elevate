package elevate.core.strategies

import elevate.core.RewriteResult._
import elevate.core.macros._
import elevate.core.{RewriteResult, Strategy}

object debug:
  def peek[P](f: P => Unit): Strategy[P] =
    rule("peek(f)", p => { f(p); Success(p) })
  
  def debug[P](msg: String): Strategy[P] =
    rule("debug", peek[P](x => println(msg + "\n" + x)))
  
  def echo[P](msg: String): Strategy[P] =
    rule("echo", peek[P](_ => println(msg)))
