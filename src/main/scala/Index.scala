import collection.mutable.HashMap
import java.io._
import scala.io.{BufferedSource, Source}
import scala.util.Marshal
import scala.xml.pull._
import scala.xml._

object Index extends WikiIndex {
  // TODO store these maps along with the index
  val titleMap = new HashMap[Int, String]
  val tokenMap = new HashMap[Int, String]

  def indexText(text: String, id: Int): Index = {
    val terms = getTerms(text)

    val index = new HashMap[Int, Seq[IndexEntry]]()
    for ((word, position) <- terms.view.zipWithIndex) {
      val token = word.hashCode
      // TODO this hashCode is only 32 bit Ints
      tokenMap(word.hashCode) = word

      index(token) = if (index.contains(token)) {
        index(token) ++ Seq((id, position))
      } else {
        Seq((id, position))
      }
    }
    index
  }

  def combineIndices(bigger: Index, smaller: Index): Index = {
    for ((term, entries) <- smaller) {
      bigger(term) = if (bigger.contains(term)) {
        bigger(term) ++ entries
      } else {
        entries
      }
    }
    bigger
  }

  def addPageToIndex(index: Index, page: Page): Index = {
    val (id, title, text) = page
    if (id % 10000 == 0) println(id + " " + title)
    titleMap(id) = title
    val pageIndex = indexText(text, id)
    combineIndices(index, pageIndex)
  }

  def parse(xml: XMLEventReader, index: Index) {
    val page = new StringBuilder()
    val id = new StringBuilder()
    val title = new StringBuilder()
    def loop(currNode: List[String]) {
      if (xml.hasNext) {
        xml.next match {
          case EvElemStart(_, label, _, _) =>
            loop(label :: currNode)
          case EvElemEnd(_, label) =>
            if (label == "page") {
              addPageToIndex(index, (id.toString.trim.toInt, title.toString, page.toString))
              id.delete(0, id.length)
              title.delete(0, title.length)
              page.delete(0, page.length)
            }
            loop(currNode.tail)
          case EvText(text) =>
            if (currNode.length > 0 && currNode(0) == "text") {
              page ++= " " + text
            }
            if (currNode.length > 0 && currNode(0) == "title") {
              title ++= text
              page ++= " " + text
            }
            if (currNode.length > 1 && currNode(0) == "id" && currNode(1) == "page") {
              id ++= text
            }
            loop(currNode)
          case _ => loop(currNode)
        }
      }
    }
    loop(List.empty)
  }

  def parse(tsv: BufferedSource, index: Index) {
    for (line <- tsv.getLines()) {
      val fields = line.split("\t")
      val page = (fields(0).toInt, fields(1), fields(2))
      addPageToIndex(index, page)
    }
  }

  def main(args: Array[String]) = {
    val wikipedia = args(0)
    val source = Source.fromFile(wikipedia)
    val index = new HashMap[Int, Seq[IndexEntry]]()

    // val xml = new XMLEventReader(source)
    parse(source, index)

    val numTokens = index.size
    println("num tokens: " + numTokens)
    val aveTokenLength = index.map{ indexEntry: (Int, Seq[IndexEntry]) =>
      tokenMap(indexEntry._1).length
    }.sum/numTokens
    println("token len: " + aveTokenLength)
    println("titles: " + titleMap.size)

    dumpIndex(index, indexFileName)
    dumpMap(tokenMap.toMap, tokenFileName)
    dumpMap(titleMap.toMap, titleFileName)
  }
}
