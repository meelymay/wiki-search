import collection.mutable.HashMap
import scala.io.{BufferedSource, Source}

class Index {
  import Index._

  val stopFilename = "src/main/resources/stop.txt"
  val stop = Source.fromFile(stopFilename).getLines.toSet

  val titleMap = new HashMap[Int, String]
  val tokenMap = new HashMap[Int, String]
  val index: DocIndex = new HashMap[Int, Seq[IndexEntry]]
}

object Index {
  type IndexEntry = (Int, Int)
  type DocIndex = HashMap[Int, Seq[IndexEntry]]
}