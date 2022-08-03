package elevate.heuristic_search

import elevate.heuristic_search.util.Solution

import java.security.MessageDigest
//import java.util.HexFormat

package object util {

  val sha256 = MessageDigest.getInstance("SHA-256")
  //  val sha64 = MessageDigest.getInstance("SHA-64")

  def convertBytesToHex(bytes: Seq[Byte]): String = {
    val sb = new StringBuilder
    for (b <- bytes) {
      sb.append(String.format("%02x", Byte.box(b)))
    }
    sb.toString
  }


  def hashAndNumbers[P](solutionHash: String, rewrites: Seq[Int]): String = {
    val solutionString = solutionHash + rewrites.mkString(":")

    val hash = sha256.digest(solutionString.getBytes("UTF-8"))

    // return hex string
    //    HexFormat.of().formatHex(hash)
    convertBytesToHex(hash.toSeq)
  }

  // implement hashing based on rewrites of solution
  def hashSolution[P](solution: Solution[P]): String = {

    //    val programString = solution.expression().toString
    //    val strategyString = solution.strategies().mkString(":")

    //    val solutionString = programString + strategyString

    // todo make sure different but equal paths become same hash
    // some form of sorting?
    // remove ids?

    // remove ids
    // could lead to same but we don't know
    val filtered = solution.rewrites().filter(elem => !elem.strategy.equals(elevate.core.strategies.basic.id[P]))

    val solutionString = filtered.mkString(",")

    val hash = sha256.digest(solutionString.getBytes("UTF-8"))

    // return hex string
    //    HexFormat.of().formatHex(hash)

    convertBytesToHex(hash.toSeq)
  }

  //  def hashSolution[P](solution: Solution[P]): String = {
  //
  //    val programString = solution.expression().toString
  //    val strategyString = solution.strategies().mkString(":")
  //
  //    val solutionString = programString + strategyString
  //
  //    val hash = sha256.digest(solutionString.getBytes("UTF-8"))
  //
  //    // return hex string
  //    //    HexFormat.of().formatHex(hash)
  //
  //    convertBytesToHex(hash.toSeq)
  //  }

  // todo replace this in code if possible
  def hashProgram[P](program: P): String = {

    val programString = program.toString

    val hash = sha256.digest(programString.getBytes("UTF-8"))

    // return hex string
    //    HexFormat.of().formatHex(hash)
    convertBytesToHex(hash.toSeq)
  }

}
