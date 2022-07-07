package elevate.exploration

import elevate.core.Strategy
import elevate.heuristic_search.Metaheuristic
import elevate.heuristic_search.heuristic.IterativeImprovement
import elevate.heuristic_search.heuristics.{Random, RandomSampling}
import elevate.heuristic_search.util.Solution

import java.nio.file.{Files, Paths}
import scala.collection.immutable.Range
import scala.collection.mutable.ListBuffer
import scala.sys.process._
import scala.language.postfixOps

// todo implement this object
object simpleRewriteExploration {
  val depth = 10
  val strategies = 10
  val seeed = 1800

  val r = new scala.util.Random(seeed)

  // todo check if we can switch to seq
  val rewriteFunction: Solution[SimpleRewrite] => Set[Solution[SimpleRewrite]] = solution => {


    // todo handle 0 strategy if necessary
    val NSize = r.nextInt(strategies)

    val N = new ListBuffer[Solution[SimpleRewrite]]

    //    println("NSize: " + NSize)
    for (_ <- Range(0, NSize)) {

      val strategy = r.nextInt(strategies)
      // todo append strategies as Ints?
      val result = Solution[SimpleRewrite](solution.expression :+ strategy, solution.strategies)
      N.addOne(result)
      //      println("result: " + result.expression.mkString("[", ", ", "]"))
    }


    N.toSet
  }

  def explore(): Unit = {

    // create metaheuristic

    // create unique output folder
    val uniqueFilename_full = uniqueFilename("exploration" + "/" + "results" + "/" + "simple_rewrite")
    (s"mkdir -p ${uniqueFilename_full}" !!)

    val nameList = scala.collection.mutable.ListBuffer.empty[String]
    var predecessor = uniqueFilename_full
    predecessor = predecessor
    nameList += predecessor

    // create folder for executor
    val executorOutput = predecessor + "/" + "Executor"

    // create subfolders
    nameList.foreach(elem => {
      println("elem: " + elem)
      (s"mkdir -p ${elem}" !!)
      (s"mkdir -p ${elem}" + "/Expressions" !!)
    })

    // create subfolder for executor
    println("elem: " + executorOutput)
    (s"mkdir ${executorOutput}" !!)


    // create executor
    val executor = RandomExecutor(
      output = executorOutput
    )

    val exploration = new Metaheuristic[SimpleRewrite](
      name = "simple rewrite",
      //      heuristic = new IterativeImprovement[SimpleRewrite],
      heuristic = new Random[SimpleRewrite],
      depth = 1000,
      iterations = 1,
      runner = executor,
      strategies = Set.empty[Strategy[SimpleRewrite]],
      output = nameList.last,
      rewriteFunction = Some(rewriteFunction),
      afterRewrite = None,
      importExport = None
    )

    val id: Strategy[SimpleRewrite] = elevate.core.strategies.basic.id[SimpleRewrite]

    val solution = Solution[SimpleRewrite](Seq.empty[Int], Seq(id))

    exploration.execute(solution)

  }


  // now create exploration


  //    val N = scala.collection.mutable.Set.empty[Solution[SimpleRewrite]]
  //    for (i <- Range(0, solution.expression.size)) {
  //      // create new solutions appendix possible rewrites
  //
  //      // append: 1 2 3 4
  //      // append: 1 2 3
  //      // append: 1 2
  //      // append: 1
  //
  //      // result
  //      // [[0]]
  //      // [[0, 1], [0, 2], [0, 4], [0, 4]]
  //      // [[0, 1, 1], [0, 1, 2], [0, 1, 3]], ...
  //      // [[0, 1, 1, 1], [0, 1, 2, 1], [0, 1, 3, 1]], ...
  //
  //    }

  def uniqueFilename(path: String): String = {
    // check if output path already exists
    var uniqueFilename_full = path
    if (Files.exists(Paths.get(uniqueFilename_full))) {
      // wait for next millisecond to create unique filename using timestamp
      Thread.sleep(1)
      uniqueFilename_full = uniqueFilename_full + "_" + System.currentTimeMillis()
    }

    uniqueFilename_full
  }

}
