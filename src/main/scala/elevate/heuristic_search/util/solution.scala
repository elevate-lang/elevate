package elevate.heuristic_search.util

import elevate.core.Strategy

class Solution[P](val expression:P,
                  val strategies:Seq[Strategy[P]]
                 ){
}
