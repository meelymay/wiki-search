import collection.mutable.HashMap
import java.io._
import scala.util.Marshal
import scala.io.{BufferedSource, Source}

trait SerializeIndex extends Index {
  import Index._

  val indexFilename = "wikipedia_index.out"
  val tokenFilename = "wikipedia_tokens.out"
  val titleFilename = "wikipedia_titles.out"

  /*
   * Serializing index
   */

  def entryToString(indexEntry: IndexEntry): String = {
    indexEntry._1 + ":" + indexEntry._2
  }

  def indexEntryToString(entry: (Int, Seq[IndexEntry])): String = {
    val term = entry._1
    val docs = entry._2
    val s = new StringBuilder(term + "\t")
    for (doc: IndexEntry <- docs) {
      s.append(entryToString(doc) + ",")
    }
    s.toString
  }

  def dumpIndex(filename: String) {
      // val out = new FileOutputStream(filename)
      val out = new PrintWriter(new File(filename))
      // out.write(Marshal.dump(index))

      for (entry: (Int, Seq[IndexEntry]) <- index) {
        out.write(indexEntryToString(entry) + "\n")
      }

      out.close
  }

  /*
   * Deserializing index
   */

  def parseEntries(entries: String): Seq[IndexEntry] = {
    entries.split(",").map { entry: String =>
      val docPosition = entry.split(":")
      if (docPosition.size == 2) {
        (docPosition(0).toInt, docPosition(1).toInt)
      } else {
        (0,0)
      }
    }.toSeq
  }

  def loadIndex(filename: String) {
    // TODO this is fucked heap for some reason :-/
    val in = Source.fromFile(filename)
    // val bytes = in.map(_.toByte).toArray
    // Marshal.load[Index](bytes)

    for (line <- in.getLines) {
      val term = line.split("\t")
      index.put(term(0).toInt, parseEntries(term(1)))
    }
    in.close()
  }

  /*
   * Serialize tokens/titles
   */

  def idNameToString(idString: (Int, String)): String = {
    idString._1 + ":" + idString._2
  }

  def dumpMap(idMap: Map[Int, String], filename: String) {
    val out = new PrintWriter(new File(filename))
    for (idString: (Int, String) <- idMap) {
      out.write(idNameToString(idString) + "\n")
    }
    out.close
  }

  /*
   * Deserialize tokens/titles
   */

  def parseIdString(idString: String): (Int, String) = {
    val idName = idString.split(":")
    if (idName.size == 2) {
      (idName(0).toInt, idName(1))
    } else {
      (0, "")
    }
  }

  def loadIdMap(filename: String): Seq[(Int, String)] = {
    val in = Source.fromFile(filename)
    val idMap = in.getLines.map { line =>
      println(line)
      parseIdString(line)
    }
    in.close()

    idMap.toSeq
  }

  def loadTokenMap() {
    for ((id, name) <- loadIdMap(tokenFilename)) {
      tokenMap.put(id, name)
    }
  }

  def loadTitleMap() {
    for ((id, name) <- loadIdMap(titleFilename)) {
      titleMap.put(id, name)
    }
  }

  /**
   * Store the index in index, title, and token files
   */
  // indexFilename: String, titleFilename: String, tokenFilename: String
  def serialize() {
    dumpIndex(indexFilename)
    dumpMap(tokenMap.toMap, tokenFilename)
    dumpMap(titleMap.toMap, titleFilename)
  }

  /**
   * Load the index from index, title, and token files
   */
  def deserialize() {
    loadTitleMap()
    loadTokenMap()
    loadIndex(indexFilename)
  }
}