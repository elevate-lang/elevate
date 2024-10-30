//package elevate.heuristic_search.heuristics
//
//trait GameState {
//  def getLegalActions: List[GameAction]
//
//  def move(action: GameAction): GameState
//
//  def isTerminal: Boolean
//
//  def getReward: Double
//}
//
//case class GameAction(index: Int)
//
//import scala.util.Random
//
//case class Node(state: GameState, parent: Option[Node], action: Option[GameAction]) {
//  var wins: Double = 0
//  var visits: Int = 0
//  val children: collection.mutable.ListBuffer[Node] = collection.mutable.ListBuffer()
//
//  def uctValue(explorationParameter: Double = Math.sqrt(2)): Double = {
//    if (visits == 0) Double.MaxValue
//    else wins.toDouble / visits + explorationParameter * Math.sqrt(Math.log(parent.map(_.visits.toDouble).getOrElse(1.0)) / visits)
//  }
//
//  def addChild(childState: GameState, action: GameAction): Node = {
//    val childNode = Node(childState, Some(this), Some(action))
//    children += childNode
//    childNode
//  }
//
//  def isFullyExpanded: Boolean = children.size == state.getLegalActions.size
//
//  def bestChild: Node = children.maxBy((child: Node) => child.wins.toDouble / child.visits)
//
//
//  def selectPromisingNode: Node = if (isFullyExpanded) children.maxBy(_.uctValue()) else this
//}
//
//class MCTS(iterationLimit: Int) {
//  def search(initialState: GameState): GameState = {
//    val rootNode = Node(initialState, None, None)
//
//    for (_ <- 1 to iterationLimit) {
//      val promisingNode = selectPromisingNode(rootNode)
//      val expandedNode = expandNode(promisingNode)
//      val simulationResult = simulateRandomPlayout(expandedNode)
//      backPropagate(expandedNode, simulationResult)
//    }
//
//    printTree(rootNode, 0)
//
//    rootNode.bestChild.state
//  }
//
//  private def selectPromisingNode(node: Node): Node = {
//    var currentNode = node
//    while (!currentNode.state.isTerminal && currentNode.isFullyExpanded) {
//      currentNode = currentNode.children.maxBy(_.uctValue())
//    }
//    currentNode
//  }
//
//  private def expandNode(node: Node): Node = {
//    val untriedActions = node.state.getLegalActions.filterNot(action => node.children.exists(_.action.contains(action)))
//    if (untriedActions.nonEmpty) {
//      val action = untriedActions.head
//      val newState = node.state.move(action)
//      node.addChild(newState, action)
//    } else {
//      node
//    }
//  }
//
//  private def simulateRandomPlayout(node: Node): Double = {
//    var tempState = node.state
//    while (!tempState.isTerminal) {
//      val legalActions = tempState.getLegalActions
//      val randomAction = legalActions(Random.nextInt(legalActions.size))
//      tempState = tempState.move(randomAction)
//    }
//    tempState.getReward
//  }
//
//  private def backPropagate(node: Node, reward: Double): Unit = {
//    var tempNode: Option[Node] = Some(node)
//    while (tempNode.isDefined) {
//      tempNode.get.visits += 1
//      tempNode.get.wins += reward
//      tempNode = tempNode.get.parent
//    }
//  }
//
//  def printTree(node: Node, depth: Int = 0): Unit = {
//    // Print the current node with indentation based on its depth in the tree
//    println(" " * depth * 2 + node.state + " " + node.wins + " " + node.visits)
//
//    // Recursively print all the children
//    for (child <- node.children) {
//      printTree(child, depth + 1)
//    }
//  }
//}
//
//
//case class DummyGameState(currentPlayer: Int, movesLeft: Int) extends GameState {
//  override def getLegalActions: List[GameAction] = (1 to movesLeft).map(GameAction).toList
//
//  override def move(action: GameAction): GameState = DummyGameState(-currentPlayer, movesLeft - 1)
//
//  override def isTerminal: Boolean = movesLeft == 0
//
//  override def getReward: Double = if (movesLeft % 2 == 0) currentPlayer else -currentPlayer
//}
//
//object Main extends App {
//  val initialState = DummyGameState(1, 3)
//  val mcts = new MCTS(iterationLimit = 10)
//  val finalState = mcts.search(initialState)
//
//  println(s"Best move leads to state: $finalState")
//}
