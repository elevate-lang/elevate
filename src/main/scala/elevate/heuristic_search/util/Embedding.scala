package elevate.heuristic_search.util

import scala.collection.mutable.ListBuffer
import elevate.core.{RewriteResult, Strategy}
import elevate.heuristic_search.HeuristicPanel

import scala.sys.process._
import scala.language.postfixOps


class EmbeddingElement[P](
                           override val solution: Solution[P],
                           override val value: Option[Double],
                         ) extends SearchSpaceElement[P](solution, value) {

}

class Embedding[P](
                    var searchSpace: ListBuffer[SearchSpaceElement[P]] = ListBuffer.empty[SearchSpaceElement[P]],
                    val panel: HeuristicPanel[P]
                    //                    writeSolution: (Solution[P], String) => Unit,
                    //                    readSolution: String => Solution[P]
                  ) extends SearchSpace[P] {


  override def add(solution: Solution[P], value: Option[Double]): Unit = {

    searchSpace.addOne(
      new EmbeddingElement(
        solution,
        value
      )
    )

  }

  override def printConsole(): Unit = ???

  override def getSizeTotal(): Int = ???

  override def getSize(): Int = {
    searchSpace.size
  }

  override def getSearchSpace(): Seq[Solution[P]] = {
    searchSpace.toSeq.map(e => e.solution)
  }

  override def getElement(i: Int): SearchSpaceElement[P] = ???

  override def writeToDot(filename: String): String = ???

  override def writeToDisk(filename: String): Unit = ???

  override def writeSearchSpace(filename: String): Unit = {

    // read search space

    val test = panel.importSolution(filename)

    println("test: \n" + test)

    System.exit(0)

    // create output folder
    (s"mkdir -p ${filename} " !!)

    searchSpace.foreach(elem => {
      // create folder foreach expression
      val solutionFolder = filename + "/" + hashSolution(elem.solution)
      (s"mkdir -p ${solutionFolder}" !!)

      // write solution to file
      panel.exportSolution(elem.solution, solutionFolder)

    })

    // write additional information?

  }

  def readSearchSpace(filename: String): Seq[Solution[P]] = {

    // create output expression foreach folder in filename

    // create solution from
    // expression in folder (file)
    // strategies in folder (file)

    // read in additional information

    Seq.empty[Solution[P]]
  }

}
