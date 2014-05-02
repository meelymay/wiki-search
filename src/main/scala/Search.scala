import collection.mutable.HashMap
import java.io._
import scala.io.Source
import scala.util.Marshal
import scala.xml.pull._
import scala.xml._

object Search extends WikiIndex {
  val stop = Source.fromFile("src/main/resources/stop2.txt").getLines.toSet
  val titleMap = new HashMap[Int, String]
  val tokenMap = new HashMap[Int, String]

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

    val index = new HashMap[String, Seq[IndexEntry]]()
    for ((word, position) <- terms.view.zipWithIndex) {
      index(word) = if (index.contains(word)) {
        Seq((id, index(word)(0)._2 ++ Seq(position)))
        // index(word) ++ Seq((id, position))
      } else {
        Seq((id, Seq(position)))
        // Seq((id, position))
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

    val indexFileName = "wikipedia_index.out"

    if (args.length == 0) {
      val wikipedia = "src/main/resources/wiki20k.xml"
      val source = Source.fromFile(wikipedia)
      val index = new HashMap[String, Seq[IndexEntry]]()

      val xml = new XMLEventReader(source)
      parse(xml, index)

      val numTokens = index.size
      println("num tokens: " + numTokens)
      val aveTokenLength = index.map(_._1.length).sum/numTokens
      println("token len: " + aveTokenLength)

      dumpIndex(index, indexFileName)
    } else {
      val index = loadIndex(indexFileName)

      for (query <- args) {
        println("Searching for " + query)
        val ids = search(query, index)
        for (id <- ids) {
          println("\t" + id)
        }
      }
    }
  }
}

trait WikiIndex {
  type IndexEntry = (Int, Seq[Int])
  // type IndexEntry = (Int, Int)
  type Index = HashMap[String, Seq[IndexEntry]]
  type Page = (Int, String)

  def dumpIndex(index: Index, filename: String) {
      val out = new FileOutputStream(filename)
      out.write(Marshal.dump(index))
      out.close
  }

  def loadIndex(filename: String): Index = {
      val in = new FileInputStream(filename)
      val bytes = Stream.continually(in.read).takeWhile(-1 != _).map(_.toByte).toArray
      Marshal.load[Index](bytes)
  }
}