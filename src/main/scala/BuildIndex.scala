import collection.mutable.HashMap
import java.io._
import scala.annotation.tailrec
import scala.io.{BufferedSource, Source}
import scala.util.Marshal
import scala.xml.pull._
import scala.xml._

trait BuildIndex extends Index {
  import Index._

  type Page = (Int, String, String)

  /**
   * Utility method to get single terms from strings.
   */
  def getTerms(text: String): Seq[String] = {
    // TODO stem the terms here?
    text.split("\\s+")
      .map(_.toLowerCase.filter(Character.isLetter(_)))
      .filter(!stop.contains(_))
  }

  /**
   * Create an Index from a single article and its id
   */
  def indexText(text: String, id: Int): DocIndex = {
    val terms = getTerms(text)

    val pageIndex = new HashMap[Int, Seq[IndexEntry]]()
    for ((word, position) <- terms.view.zipWithIndex) {
      val token = word.hashCode
      // TODO this hashCode is only 32 bit Ints
      tokenMap(word.hashCode) = word

      pageIndex(token) = if (pageIndex.contains(token)) {
        pageIndex(token) ++ Seq((id, position))
      } else {
        Seq((id, position))
      }
    }
    pageIndex
  }

  /**
   * Add small index to the index.
   */
  def combineIndices(smaller: DocIndex) {
    for ((term, entries) <- smaller) {
      index(term) = if (index.contains(term)) {
        index(term) ++ entries
      } else {
        entries
      }
    }
  }

  /**
   * Create a new Index from the Page and combine with the given Index
   * Also add new terms and title to maps
   */
  def addPageToIndex(page: Page) {
    val (id, title, text) = page
    if (id % 10000 == 0) println(id + " " + title)
    titleMap(id) = title
    val pageIndex = indexText(text, id)
    combineIndices(pageIndex)
  }

  /**
   * Create an Index from XML, while populating corresponding title and token maps
   *
   * (unused, from previous challenge description)
   */
  def parse(xml: XMLEventReader) {
    val page = new StringBuilder()
    val id = new StringBuilder()
    val title = new StringBuilder()
    @tailrec def loop(currNode: List[String]) {
      if (xml.hasNext) {
        xml.next match {
          case EvElemStart(_, label, _, _) =>
            loop(label :: currNode)
          case EvElemEnd(_, label) =>
            if (label == "page") {
              addPageToIndex((id.toString.trim.toInt, title.toString, page.toString))
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

  /**
   * Create an Index from a TSV, while populating corresponding title and token maps
   */
  def parse(tsv: BufferedSource) {
    for (line <- tsv.getLines()) {
      val fields = line.split("\t")
      val page = (fields(0).toInt, fields(1), fields(2))
      addPageToIndex(page)
    }
  }

  /**
   * Print some stats about the index created
   */
  def printIndexStats() {
    val numTokens = index.size
    println("num tokens: " + numTokens)
    val aveTokenLength = index.map{ indexEntry: (Int, Seq[IndexEntry]) =>
      tokenMap(indexEntry._1).length
    }.sum/numTokens
    println("token len: " + aveTokenLength)
    println("titles: " + titleMap.size)
  }
}

object BuildMain {
  def main(args: Array[String]) = {
    val filename = args(0)
    val source = Source.fromFile(filename)

    val wikipedia = new Index with BuildIndex with SerializeIndex

    // populate the index and maps by parsing the tsv source
    wikipedia.parse(source)
    wikipedia.printIndexStats()

    // store the index
    wikipedia.serialize()
  }
}