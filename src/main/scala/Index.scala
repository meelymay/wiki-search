import collection.mutable.HashMap
import scala.io.{BufferedSource, Source}

class Index {
  import Index._

  val titleMap = new HashMap[DocId, String]
  val docSizeMap = new HashMap[DocId, Int]
  val tokenMap = new HashMap[Token, String]
  val index: DocIndex = new HashMap[Token, Seq[IndexEntry]]
}

object Index {
  type Token = Int
  type Position = Int
  type DocId = Int
  type IndexEntry = (DocId, Position)
  type DocIndex = HashMap[Token, Seq[IndexEntry]]

  val stopFilename = "src/main/resources/stop.txt"
  val stop = Source.fromFile(stopFilename).getLines.toSet

  /**
   * Utility method to get single terms from strings.
   */
  def getTerms(text: String): Seq[String] = {
    // TODO stem the terms here?
    text.split("\\s+")
      .map(_.toLowerCase.filter(Character.isLetter(_)))
      .filter(!stop.contains(_))
  }
}