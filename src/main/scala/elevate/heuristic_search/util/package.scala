package elevate.heuristic_search

import java.security.MessageDigest
import java.util.HexFormat

package object util {

  val sha256 = MessageDigest.getInstance("SHA-256")

  def hashProgram[P](program: P): String = {

    val programString = program.toString

    val hash = sha256.digest(programString.getBytes("UTF-8"))

    // return hex string
    HexFormat.of().formatHex(hash)
  }

}
