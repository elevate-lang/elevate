package elevate.heuristic_search

import java.security.MessageDigest
//import java.util.HexFormat

package object util2 {

  val sha256 = MessageDigest.getInstance("SHA-256")

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

  def hashSolution[P](solution: Solution[P]): String = {

    val programString = solution.expression.toString
    val strategyString = solution.strategies.mkString(":")

    val solutionString = programString + strategyString

    val hash = sha256.digest(solutionString.getBytes("UTF-8"))

    // return hex string
    //    HexFormat.of().formatHex(hash)

    convertBytesToHex(hash.toSeq)
  }

  def hashProgram[P](program: P): String = {

    val programString = program.toString

    val hash = sha256.digest(programString.getBytes("UTF-8"))

    // return hex string
    //    HexFormat.of().formatHex(hash)
    convertBytesToHex(hash.toSeq)
  }

}
