package elevate

import elevate.core.strategies.basic._

import scala.language.implicitConversions

package object core {

  type Strategy[P] = P => RewriteResult[P]

  implicit class Then[P](f: Strategy[P]) {
    def `;`(s: Strategy[P]): Strategy[P] = seq[P](f)(s) //scalastyle:ignore
  }

  implicit class LeftChoice[P](f: Strategy[P]) {
    def <+(s: Strategy[P]): Strategy[P] = leftChoice(f)(s)
  }
}
