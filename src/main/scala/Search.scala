import collection.mutable.HashMap
import java.io._
import scala.io.{BufferedSource, Source}
import scala.util.Marshal
import scala.xml.pull._
import scala.xml._

object Search extends WikiIndex {
  val titleMap = loadIdMap(titleFileName)
  val tokenMap = loadIdMap(tokenFileName)

  def search(query: String, index: Index): Seq[String] = {
    val terms = getTerms(query)
    val term = terms(0)
    val token = term.hashCode
    index.getOrElse(token, Seq()).groupBy(_._1).map { document: (Int, Seq[(Int, Int)]) =>
      titleMap(document._1)
    }.toSeq
  }

  def main(args: Array[String]) = {
    val index = loadIndex(indexFileName)

    println("title map size: " + titleMap.size)
    println("token map size: " + tokenMap.size)

    for (query <- args) {
      println("Searching for " + query)
      val ids = search(query, index)
      for (id <- ids) {
        println("\t" + id)
      }
    }
  }
}