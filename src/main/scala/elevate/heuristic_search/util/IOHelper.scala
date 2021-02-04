package elevate.heuristic_search.util

import java.nio.file.{Files, Paths}

object IOHelper:

  def getUniqueFilename(filename:String, offset: Int):String=
    var uniqueFilename_full = filename

    // check if file or folder already exists
    if(Files.exists(Paths.get(uniqueFilename_full)))
      val warningString = "Warning! Clash at " + uniqueFilename_full + ".\n"
      println(warningString + "adding System.currentTimeMillis().")

      // append timestamp
      val end = uniqueFilename_full.substring(uniqueFilename_full.length-offset, uniqueFilename_full.length)
      uniqueFilename_full = uniqueFilename_full.substring(0, uniqueFilename_full.length-offset)+ "_" + System.currentTimeMillis() + end
    end if
    
    uniqueFilename_full
