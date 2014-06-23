import collection.mutable.MutableList
import java.io._
import scala.annotation.tailrec
import scala.io.{BufferedSource, Source}
import scala.util.Marshal
import scala.xml.pull._
import scala.xml._

trait BuildIndex extends Index {
  import Index._

  type Page = (DocId, String, String)

  /**
   * Create an Index from a single article and its id
   */
  def indexText(text: String, id: DocId) {
    val terms = getTerms(text)
    docSizeMap.put(id, terms.size)

    for ((word, position) <- terms.view.zipWithIndex) {
      val token = word.hashCode
      // TODO this hashCode is only 32 bit Ints
      tokenMap(word.hashCode) = word

      if (!index.contains(token)) {
        index(token) = new MutableList[IndexEntry]()
      }
      val entry = (id, position)
      index(token) += entry
    }
  }

  /**
   * Add small index to the index.
   */
  def combineIndices(smaller: DocIndex) {
    for ((term, entries) <- smaller) {
      if (index.contains(term)) {
        index(term) ++= entries
      } else {
        index(term) = entries
      }
    }
  }

  /**
   * Create a new Index from the Page and combine with the given Index
   * Also add new terms and title to maps
   */
  def addPageToIndex(page: Page) {
    val (id, title, text) = page
    // if (id % 5000 == 0) println(id + " " + title)
    titleMap(id) = title
    val start = System.nanoTime()
    val pageIndex = indexText(text, id)
    val start2 = System.nanoTime()
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
