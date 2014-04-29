import collection.mutable.HashMap
import scala.io.Source
import scala.xml.pull._
import scala.xml._

object Search {
  type IndexEntry = (Int, Seq[Int])
  type Index = HashMap[String, Seq[IndexEntry]]
  type Page = (Int, String)

  val stop = Source.fromFile("src/main/resources/stop2.txt").getLines.toSet
  val titleMap = new HashMap[Int, String]

  def getTerms(text: String): Seq[String] = {
    // TODO stem the terms here?
    text.split("\\s+")
      .map(_.toLowerCase)
      .filter(!stop.contains(_))
  }

  def indexText(text: String, id: Int): Index = {
    val terms = getTerms(text)
    /* val occurrences = terms.zipWithIndex
    val group = occurrences.groupBy(_._1)
    group.mapValues { vals: Seq[(String, Int)] =>
      Seq((id, vals.map(_._2)))
    } */

    val index = new HashMap[String, Seq[(Int, Seq[Int])]]()
    /* {
      override def default(key: String) { Seq[(Int, Seq[Int])]((id, Seq[Int]())) }
    } */
    for ((word, position) <- terms.view.zipWithIndex) {
      index(word) = if (index.contains(word)) {
        Seq((id, index(word)(0)._2 ++ Seq(position)))
      } else {
        Seq((id, Seq(position)))
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

  def search(query: String, index: Index): Seq[Int] = {
    val terms = getTerms(query)
    val term = terms(0)
    index.getOrElse(term, Seq()).map(_._1)
  }

  def addPageToIndex(index: Index, page: Page): Index = {
    val (id, text) = page
    val pageIndex = indexText(text, id)
    combineIndices(index, pageIndex)
  }

  def parse(xml: XMLEventReader, index: Index) {
    val page = new StringBuilder()
    val id = new StringBuilder()
    def loop(currNode: List[String]) {
      if (xml.hasNext) {
        xml.next match {
          case EvElemStart(_, label, _, _) =>
            loop(label :: currNode)
          case EvElemEnd(_, label) =>
            if (label == "page") {
              addPageToIndex(index, (id.toString.trim.toInt, page.toString))
              id.delete(0, id.length)
              page.delete(0, page.length)
            }
            loop(currNode.tail)
          case EvText(text) =>
            if (currNode.length > 0 && currNode(0) == "text") {
              page ++= " " + text
            }
            if (currNode.length > 0 && currNode(0) == "title") {
              println(text)
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

  def main(args: Array[String]) = {

    val wikipedia = "src/main/resources/wiki100k.xml"
    val source = Source.fromFile(wikipedia) // XML.loadFile(anarchism)
    val index = new HashMap[String, Seq[(Int, Seq[Int])]]()

    val xml = new XMLEventReader(source)
    parse(xml, index)

    for (query <- args) {
      println("Searching for " + query)
      val ids = search(query, index)
      for (id <- ids) {
        println("\t" + id)
      }
    }

    println("Done.")
  }
}