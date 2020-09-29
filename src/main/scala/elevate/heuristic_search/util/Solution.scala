package elevate.heuristic_search.util

import elevate.core.Strategy

import scala.collection.immutable

case class Solution[P](expression: P, strategies: immutable.Seq[Strategy[P]])
