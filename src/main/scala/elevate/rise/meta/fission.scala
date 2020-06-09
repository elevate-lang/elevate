package elevate.rise.meta

import elevate.core.strategies.basic._
import elevate.core.{Strategy, Success}
import elevate.macros.RuleMacro.rule
import elevate.rise.Rise
import elevate.rise.meta.traversal._
import elevate.rise.rules.traversal.{argument, argumentOf, body, function}

object fission {

  @rule def bodyFission: Strategy[Strategy[Rise]] = {
    case body(seq(f,s)) => Success(seq(body(f),body(s)))
  }

  @rule def functionFission: Strategy[Strategy[Rise]] = {
    case function(seq(f,s)) => Success(seq(function(f),function(s)))
  }

  @rule def argumentFission: Strategy[Strategy[Rise]] = {
    case argument(seq(f,s)) => Success(seq(argument(f),argument(s)))
  }

  @rule def argumentOfFission: Strategy[Strategy[Rise]] = {
    case argumentOf(x,seq(f,s)) => Success(seq(argumentOf(x,f), argumentOf(x,s)))
  }

  // Fissioned-Normal-Form: Every single strategy application starts from the root
  def FNF: Strategy[Strategy[Rise]] =
    normalize.apply(bodyFission <+ functionFission <+ argumentFission <+ argumentOfFission)
}
