package elevate.heuristic_search.util

import elevate.core.{Strategy}

import java.nio.file.{Files, Paths}
import scala.collection.mutable.ListBuffer
import scala.language.postfixOps
import scala.sys.process._

abstract class SearchSpaceElement[P](val solution: Solution[P],
                                     val value: Option[Double]){
}

trait SearchSpace[P] {

  def add(program: P, strategy: Strategy[P], value:Option[Double]): Unit

  def printConsole(): Unit

  // get total size of elements in structure
  def getSizeTotal(): Int

  // get numnber of unique nodes in structure (unique nodes vs. leafs)
  def getSize(): Int

  // get all unique nodes or leafs of tree
  def getSearchSpace(): Seq[Solution[P]]

  // get i element from get search space
  def getElement(i: Int): SearchSpaceElement[P]

  def writeToDot(filename: String): String

  def writeToDisk(filename: String): Unit

}

object SearchSpaceHelper{

  def getUniqueFilename(filename:String, offset: Int):String= {
    var uniqueFilename_full = filename

    // check if file or folder already exists
    if(Files.exists(Paths.get(uniqueFilename_full))){
      //val warningString = "Warning! Clash at " + uniqueFilename_full + ".\n"
      //println(warningString + "adding System.currentTimeMillis().")

      // wait for it
      Thread.sleep(1)

      // append timestamp
      val end = uniqueFilename_full.substring(uniqueFilename_full.length-offset, uniqueFilename_full.length)
      uniqueFilename_full = uniqueFilename_full.substring(0, uniqueFilename_full.length-offset)+ "_" + System.currentTimeMillis() + end
    }

    uniqueFilename_full
  }

  val strategies2: Map[String, Int] = Map(
    "id" -> 0,
    "topDownSplitJoin" -> 1,
  )

  val strategies: Map[String, Int] = Map(
    "id" -> 0,
    "lowerGs0" -> 1,
    "lowerGs1" -> 2,
    "lowerWrg0" -> 3,
    "lowerWrg1" -> 4,
    "lowerLcl0" -> 5,
    "lowerLcl1" -> 6,
    "lowerGsGs" -> 7,
    "lowerWrgLcl" -> 8,
    "lowerWrgWrgLclLcl" -> 9,
//     split join
    "allSplitJoin" -> 10,
    "oneSplitJoin" -> 11,
    "someSplitJoin" -> 12,
    "oneUsingStateSplitJoin" -> 13,
    "topDownSplitJoin" -> 14,
    "allTopdownSplitJoin" -> 15,
    "bottomUpSplitJoin" -> 16
  )

  def getStrategies(numbers: Seq[Int]): Seq[String] = {
    val strategyBuffer = new ListBuffer[String]
    numbers.foreach(number => {
      strategyBuffer += strategies.map(_.swap).apply(number)
    })

    strategyBuffer.toSeq
  }
}
