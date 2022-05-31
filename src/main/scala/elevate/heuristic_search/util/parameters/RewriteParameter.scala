package elevate.heuristic_search.util.parameters

import elevate.core.Strategy
import elevate.heuristic_search.util.SearchSpaceHelper

case class Parameter(
                    name: String,
                    values: Seq[Int],
                    constraints: Seq[String],
                    dependencies: Seq[String] // todo implicit?
                    )

case class RewriteParameter[P](
                      name: String, // not necessary?
                      strategy: Strategy[P],
                      application: Seq[Int],
//                      parameters: String = "new parameter with divides",
                      parameters: Seq[Parameter]
//                      constraints: Seq[String] // todo think about that (and change)
                      ){

}

object test {

  // name app            param              value
  // id   0 1 8       -> [1, 1, 1]       -> id((0,1), (1,1), (8,1))
  // id2  2 3 4 5 6 7 -> [2, 2, 2, 2, 2] -> id2((2,2), (3,2), (4,2), (5,2), (6,2), (7,2))

  // dependencies beyond rewrite?
  // pick x (once) before you can take this path?
  // can we or do we need to express this?

  // complete sample
  // sample (
  //  id((0,1), (1,1), (8,1))
  //  id2((2,2), (3,2), (4,2), (5,2), (6,2), (7,2))
  // )

  // can we squeeze all information into the "parameter" thing?

  // pick rewrite values
    // pick parameters (maybe set of parameters)
  // eval cost function?

  // apply id strategy at 0, 1 and 8
  // introducing constant tuning parameter tpx
  val sample = RewriteParameter(
    name = "id",
    strategy = elevate.core.strategies.basic.id,
    application = Seq(0,1,8),
    // todo this should be generated out of information
    parameters = Seq(
      Parameter(
        name = "tp0", //
        values = Seq(1), // foreach param individually
        constraints = Seq(""),
        dependencies = Seq("")
      ),
      Parameter(
        name = "tp1", //
        values = Seq(1), // foreach param individually
        constraints = Seq(""),
        dependencies = Seq("")
      ),
      Parameter(
        name = "tp8", //
        values = Seq(1), // foreach param individually
        constraints = Seq(""),
        dependencies = Seq("")
      )
    )
  )
}


//
//  # think about dependencies among each other?
//  def __init__(
//                self,
//                name: str,
//                values: list[float],
//                default: union[float],
//                probability_distribution: list[float],
//                constraints: list[str] = [],
//  dependencies: list[str] = [],
//  transform: optional[str] = "none",
//  ):



// conclusion:

// layers are tuning parameters
// chain of trees by filling with id
// problem: creating tree is expensive
// solution: create tree directly in hm? Do we have another representation of tree?

//
// rs2 vs rs?

