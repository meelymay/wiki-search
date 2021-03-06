import collection.mutable.{HashMap, MutableList}
import java.io._
import scala.util.Marshal
import scala.io.{BufferedSource, Source}

trait SerializeIndex extends Index {
  import Index._

  val indexFilename = "src/main/resources/wikipedia_index.out"
  val tokenFilename = "src/main/resources/wikipedia_tokens.out"
  val titleFilename = "src/main/resources/wikipedia_titles.out"
  val docSizeFilename = "src/main/resources/wikipedia_sizes.out"

  /*
   * Serializing index
   */

  def entryToString(indexEntry: IndexEntry): String = {
    indexEntry._1 + ":" + indexEntry._2
  }

  def indexEntryToString(entry: (DocId, Seq[IndexEntry])): String = {
    val term = entry._1
    val docs = entry._2
    val s = new StringBuilder(term + "\t")
    for (doc: IndexEntry <- docs) {
      s.append(entryToString(doc) + ",")
    }
    s.toString
  }

  def dumpIndex(filename: String) {
    val out = new PrintWriter(new File(filename))
    for (entry: (DocId, Seq[IndexEntry]) <- index) {
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
    }
  }

  def loadIndex(filename: String) {
    // TODO this is fucked heap for some reason :-/
    val in = Source.fromFile(filename)
    // val bytes = in.map(_.toByte).toArray
    // Marshal.load[Index](bytes)

    for (line <- in.getLines) {
      val term = line.split("\t")
      val entries = new MutableList[IndexEntry]()
      entries ++= parseEntries(term(1))
      index.put(term(0).toInt, entries)
    }
    in.close()
  }

  /*
   * Serialize tokens/titles
   */

  def idNameToString(idString: (Int, String)): String = {
    idString._1 + ":" + idString._2
  }

  def idCountToString(idCount: (Int, Int)): String = {
    idCount._1 + ":" + idCount._2
  }

  def dumpMap(idMap: Map[Int, String], filename: String) {
    val out = new PrintWriter(new File(filename))
    for (idString: (Int, String) <- idMap) {
      out.write(idNameToString(idString) + "\n")
    }
    out.close
  }

  def dumpDocSize() {
    val out = new PrintWriter(new File(docSizeFilename))
    for (idString: (DocId, Int) <- docSizeMap) {
      out.write(idCountToString(idString) + "\n")
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
      (0, "0")
    }
  }

  def parseIdCount(idString: String): (Int, Int) = {
    val p = parseIdString(idString)
    p match {
      case (id, count) => (id, count.toInt)
    }
  }

  def loadIdMap(filename: String): Seq[(Int, String)] = {
    Source.fromFile(filename).getLines.map { line =>
      parseIdString(line)
    }.toSeq
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

  def loadDocSizeMap() {
    loadDocSizeMap(docSizeFilename)
  }

  def loadDocSizeMap(filename: String) {
    for (line <- Source.fromFile(filename).getLines) {
      val (id, count) = parseIdCount(line)
      docSizeMap.put(id, count)
    }
  }

  /**
   * Store the index in index, title, and token files
   */
  def serialize() {
    dumpIndex(indexFilename)
    dumpMap(tokenMap.toMap, tokenFilename)
    dumpMap(titleMap.toMap, titleFilename)
    dumpDocSize()
  }

  /**
   * Load the index from index, title, and token files
   */
  def deserialize() {
    loadDocSizeMap()
    loadTitleMap()
    loadTokenMap()
    loadIndex(indexFilename)
  }
}