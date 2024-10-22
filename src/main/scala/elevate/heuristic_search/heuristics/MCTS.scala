package elevate.heuristic_search.heuristics

import elevate.heuristic_search._
import elevate.heuristic_search.util.Solution

import scala.collection.mutable
import scala.util.Random


// class node for tree data structure
case class Node[P](
                    solution: Solution[P],
                    parent: Option[Node[P]] = None,
                    children: collection.mutable.Buffer[Node[P]] = collection.mutable.Buffer.empty[Node[P]],
                    var wins: Double = 0.0,
                    var visits: Int = 0
                  ) {

  // compute uct value
  def uctValue(totalSimulations: Double, explorationParam: Double = Math.sqrt(2)): Double = {
    if (visits == 0) {
      Double.MaxValue
    } else {
      wins / visits + explorationParam * Math.sqrt(Math.log(totalSimulations) / visits)
    }
  }

  def isTerminal(depth: Int): Boolean = {
    solution.solutionSteps.size > depth
  }

  // select child with highest uct value
  def selectChild: Node[P] = {
    children.maxBy(_.uctValue(visits))
  }

  // add a new child node
  def addChild(solution: Solution[P]): Node[P] = {

    val node = Node(
      solution = solution,
      parent = Some(this),
      wins = 0.0,
      visits = 0,
    )

    children.append(node)

    node
  }
}

class MCTS[P] extends Heuristic[P] {

  def start(panel: HeuristicPanel[P], initialSolution: Solution[P], depth: Int, samples: Int): ExplorationResult[P] = {

    // execute initial solution as default
    val default_performance: Option[Double] = panel.f(initialSolution)

    var counter: Int = 1

    // create a tree with initial solution as root
    val rootNode: Node[P] = Node(
      solution = initialSolution,
      parent = None,
      wins = 0.0,
      visits = 0
    )

    // main exploration loop
    for (_ <- 1 to samples) {

      // 1. Selection based on UCB1 value
      var node: Node[P] = rootNode
      while (node.children.nonEmpty && !node.isTerminal(depth = depth)) {
        node = node.selectChild
      }

      // 2. Expansion
      // add elements to tree, when? If we didn't visit before
      if (node.visits != 0) {

        // add all children of node
        panel.N(node.solution).foreach(child => node.addChild(child))

        // choose one of them
        // they are new so choose one of them randomly
        node = node.children(Random.nextInt(node.children.size))
      }

      // 3. Rollout
      // we start the rollout at the current node
      var rollout: (Solution[P], Option[Double]) = (node.solution, None)

      var isTerminal: Boolean = false
      while (!isTerminal && rollout._1.solutionSteps.count(step => step.strategy != elevate.core.strategies.basic.id[P]) < depth) {

        val actions = panel.N(rollout._1)
        if (actions.nonEmpty) {

          // try to consider only valid ones
          // rollout performance should be the minimum that was seen during rollout
          rollout = choose_valid_solution_randomly(panel = panel, actions = actions, rollout._2)

          // check if rollout is empty
          if (rollout._1 == null) {
            isTerminal = true
          } else {
            rollout._1.solutionSteps.foreach(step => println(s"""[${step.strategy}, ${step.location}]"""))

            // check if we have a dead end for this rollout
            if (rollout._1 == null) {
              isTerminal = true
            }
          }

        } else {
          isTerminal = true
        }
      }

      // 4. Backpropagation
      counter += 1
      while (node != null) {
        node.visits += 1

        // this can be biased by the ranges
        val win: Double = rollout._2 match {
          case Some(value) => 1 / value
          case None => 0
        }
        node.wins += win
        node = node.parent.orNull
      }
    }

    def choose_valid_solution_randomly(panel: HeuristicPanel[P], actions: Seq[Solution[P]], minimum: Option[Double]): (Solution[P], Option[Double]) = {

      def findSolution(minimum: Option[Double], attempts: Set[Solution[P]]): (Solution[P], Option[Double]) = {
        val remainingActions = actions.filterNot(attempts.contains)

        if (remainingActions.isEmpty) {
          (null.asInstanceOf[Solution[P]], None) // No valid solution found
        } else {
          val candidate: Solution[P] = remainingActions(Random.nextInt(remainingActions.size))

          // get performance of
          panel.f(candidate) match {
            case Some(value) =>
              minimum match {
                case None =>
                  (candidate, Some(value)) // Valid solution found

                case Some(minimum_value) =>
                  value <= minimum_value match {
                    case true => (candidate, Some(value))
                    case false => (candidate, Some(minimum_value))
                  }
              }
            case None => findSolution(minimum, attempts + candidate) // Add to attempts and recurse
          }
        }
      }

      findSolution(minimum = minimum, attempts = Set.empty[Solution[P]])
    }

    //
    //    def choose_valid_solution_randomly(panel: HeuristicPanel[P], actions: Seq[Solution[P]]): (Solution[P], Option[Double]) = {
    //
    //      var found_valid: Boolean = false
    //      var rollout: (Solution[P], Option[Double]) = (null.asInstanceOf[Solution[P]], None)
    //      val attempts: mutable.Set[Solution[P]] = scala.collection.mutable.Set.empty[Solution[P]]
    //
    //      while (!found_valid) {
    //
    //        val remainingActions = actions.filterNot(attempts.contains)
    //
    //        rollout = remainingActions.isEmpty match {
    //          case true => rollout
    //          case false =>
    //
    //            val candidate: Solution[P] = remainingActions(Random.nextInt(remainingActions.size))
    //            attempts.add(candidate)
    //
    //            rollout = panel.f(solution = candidate) match {
    //              case Some(value) =>
    //                found_valid = true
    //                (candidate, Some(value))
    //              case None => (candidate, None)
    //            }
    //            rollout
    //        }
    //      }
    //      rollout
    //    }

    // return dummy optimized program
    ExplorationResult(
      solution = initialSolution,
      performance = None,
      searchSpace = None
    )
  }
}
