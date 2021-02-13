package elevate.core

import elevate.core.strategies.basic._

type Strategy[P] = P => RewriteResult[P]

extension [P](f: Strategy[P])
  def `;`(s: Strategy[P]): Strategy[P] = seq[P](f)(s) //scalastyle:ignore

extension [P](f: Strategy[P])
  def <+(s: Strategy[P]): Strategy[P] = leftChoice(f)(s)
