package elevate.heuristic_search.heuristics

import elevate.heuristic_search._
import elevate.heuristic_search.util.{Solution}

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
      var rollout = node.solution

      var isTerminal: Boolean = false
      while (rollout.solutionSteps.count(step => step.strategy != elevate.core.strategies.basic.id[P]) < depth && !isTerminal) {
        val actions = panel.N(rollout)
        if (actions.nonEmpty) {
          rollout = actions(Random.nextInt(actions.size))
        } else {
          isTerminal = true
        }
      }


      // 4. Backpropagation
      val value: Option[Double] = panel.f(rollout)
      counter += 1
      while (node != null) {
        node.visits += 1

        // this can be biased by the ranges
        val win: Double = value match {
          case Some(value) => 1 / value
          case None => 0
        }
        node.wins += win
        node = node.parent.orNull
      }
    }

    // return dummy optimized program
    ExplorationResult(
      solution = initialSolution,
      performance = None,
      searchSpace = None
    )
  }
}
