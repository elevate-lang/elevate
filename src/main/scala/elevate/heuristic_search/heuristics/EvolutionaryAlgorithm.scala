package elevate.heuristic_search.heuristics

import elevate.heuristic_search
import elevate.heuristic_search.util.Solution
import elevate.heuristic_search.{Heuristic, HeuristicPanel}

class EvolutionaryAlgorithm[P] extends Heuristic[P] {


  override def start(panel: HeuristicPanel[P], solution: Solution[P], depth: Int, samples: Int): heuristic_search.ExplorationResult[P] = {

    //  Step One: Generate the initial population of individuals randomly. (First generation)
    //  Step Two: Repeat the following regenerational steps until termination:
    //  Evaluate the fitness of each individual in the population (time limit, sufficient fitness achieved, etc.)
    //  Select the fittest individuals for reproduction. (Parents)
    //  Breed new individuals through crossover and mutation operations to give birth to offspring.
    //    Replace the least-fit individuals of the population with new individuals.


    // similar to local search?
    // get mutations

    // get Neighborhood of starting element as initial population

    // Evaluate Neighborhood

    // select fittest individuals

    // get neighborhoods for each neighbor

    // repeat


    null
  }
}
