import collection.mutable.HashMap
import java.io._
import scala.util.Marshal
import scala.io.{BufferedSource, Source}

trait WikiIndex {

  type IndexEntry = (Int, Int)
  type Index = HashMap[Int, Seq[IndexEntry]]
  type Page = (Int, String, String)

  val indexFileName = "wikipedia_index.out"
  val tokenFileName = "wikipedia_tokens.out"
  val titleFileName = "wikipedia_titles.out"
  val stopFileName = "src/main/resources/stop2.txt"

  val stop = Source.fromFile(stopFileName).getLines.toSet

  def getTerms(text: String): Seq[String] = {
    // TODO stem the terms here?
    text.split("\\s+")
      .map(_.toLowerCase.filter(Character.isLetter(_)))
      // TODO remove punctuation
      .filter(!stop.contains(_))
  }

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

  def parseEntries(entries: String): Seq[IndexEntry] = {
    entries.split(",").map { entry: String =>
      val docPosition = entry.split(":")
      (docPosition(0).toInt, docPosition(1).toInt)
    }.toSeq
  }

  def dumpIndex(index: Index, filename: String) {
      // val out = new FileOutputStream(filename)
      val out = new PrintWriter(new File(filename))
      // out.write(Marshal.dump(index))

      for (entry: (Int, Seq[IndexEntry]) <- index) {
        out.write(indexEntryToString(entry) + "\n")
      }

      out.close
  }

  def idStringToString(idString: (Int, String)): String = {
    idString._1 + ":" + idString._2
  }

  def parseIdString(idString: String): (Int, String) = {
    val idName = idString.split(":")
    (idName(0).toInt, idName(1))
  }

  def loadIdMap(filename: String): Map[Int, String] = {
    val in = Source.fromFile(filename)
    val idMap = in.getLines.map { line =>
      parseIdString(line)
    }.toMap
    in.close()

    idMap
  }

  def dumpMap(idMap: Map[Int, String], filename: String) {
    val out = new PrintWriter(new File(filename))
    for (idString: (Int, String) <- idMap) {
      out.write(idStringToString(idString) + "\n")
    }
    out.close
  }

  def loadIndex(filename: String): Index = {
    // TODO this is fucked heap for some reason :-/
    val in = Source.fromFile(filename)
    // val bytes = in.map(_.toByte).toArray
    // Marshal.load[Index](bytes)

    val index = new HashMap[Int,Seq[(Int, Int)]]()
    for (line <- in.getLines) {
      val term = line.split("\t")
      index.put(term(0).toInt, parseEntries(term(1)))
    }
    in.close()

    index
  }
}