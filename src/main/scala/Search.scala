import scala.io.Source
import scala.xml._

object Search {
  type IndexEntry = (Int, Seq[Int])
  type Index = Map[String, Seq[IndexEntry]]

  def getText(page: Node): String = {
    val title = (page \\ "title").text
    println(title)
    title + (page \\ "text").text
  }

  def getId(page: Node): Int = {
    (page \\ "id")(0).text.toInt
  }

  def getTerms(text: String, stop: Set[String]): Seq[String] = {
    // TODO stem the terms here?
    text.split("\\s+")
      .map(_.toLowerCase)
      .filter(!stop.contains(_))
  }

  def indexText(text: String, id: Int, stop: Set[String]): Index = {
    val terms = getTerms(text, stop)
    val occurrences = terms.zipWithIndex
    val group = occurrences.groupBy(_._1)
    group.mapValues { vals: Seq[(String, Int)] =>
      Seq((id, vals.map(_._2)))
    }
  }

  def combineIndices(right: Index, left: Index): Index = {
    val indices = right.toSeq ++ left.toSeq
    val grouped = indices.groupBy(_._1)
    grouped.mapValues { vals: Seq[(String, Seq[IndexEntry])] =>
      vals.map(_._2).flatMap { entry: Seq[IndexEntry] => entry }
    }
  }

  def search(query: String, index: Index, stop: Set[String]): Seq[Int] = {
    val terms = getTerms(query, stop)
    val term = terms(0)
    index.getOrElse(term, Seq()).map(_._1)
  }

  def addPageToIndex(stop: Set[String])(index: Index, page: Node): Index = {
    val id = getId(page)
    val text = getText(page)
    val pageIndex = indexText(text, id, stop)
    
    combineIndices(index, pageIndex)
  }

  def main(args: Array[String]) = {
    val stop = Source.fromFile("src/main/resources/stop2.txt").getLines.toSet

    val anarchism = "src/main/resources/wiki100k.xml"
    val source = XML.loadFile(anarchism)
    val index = (source \\ "mediawiki" \\ "page").foldLeft(Map[String, Seq[(Int, Seq[Int])]]())(addPageToIndex(stop)(_, _))

    for (query <- args) {
      println(search(query, index, stop))
    }

    println("done")
  }
}