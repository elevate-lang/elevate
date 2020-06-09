package elevate

import elevate.core.strategies.basic._
import elevate.rise.Rise
import elevate.rise.strategies.normalForm._

import scala.language.implicitConversions

package object core {

  type Strategy[P] = P => RewriteResult[P]

  implicit class Then[P](f: Strategy[P]) {
    def `;`(s: Strategy[P]): Strategy[P] = seq[P](f,s) //scalastyle:ignore
  }

  // scalastyle:off
  implicit class NormalizedThen(f: Strategy[Rise]) {
    def `;;`(s: Strategy[Rise]): Strategy[Rise] = f `;` DFNF `;` s
  }
  // scalastyle:on

  implicit class LeftChoice[P](f: Strategy[P]) {
    def <+(s: Strategy[P]): Strategy[P] = leftChoice(f,s)
  }
}
