package elevate

import elevate.core.strategies.basic._

import scala.language.implicitConversions

package object core {
  var applyCount = 0L
  def countApplications[P](s: Strategy[P]): Strategy[P] = {
    p => countApplications(s(p))
  }
  def countApplications[P](r: RewriteResult[P]): RewriteResult[P] = {
    if (r.isInstanceOf[Success[P]]) {
      applyCount += 1
    }
    r
  }

  type Strategy[P] = P => RewriteResult[P]

  implicit class Then[P](f: Strategy[P]) {
    def `;`(s: Strategy[P]): Strategy[P] = seq[P](f)(s) //scalastyle:ignore
  }

  implicit class LeftChoice[P](f: Strategy[P]) {
    def <+(s: Strategy[P]): Strategy[P] = leftChoice(f)(s)
  }
}
