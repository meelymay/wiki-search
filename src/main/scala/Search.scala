import collection.mutable.HashMap
import java.io._
import scala.io.{BufferedSource, Source}
import scala.util.Marshal
import scala.xml.pull._
import scala.xml._

object Search extends WikiIndex {
  val titleMap = loadIdMap(titleFileName)
  val tokenMap = loadIdMap(tokenFileName)

  def score(positions: Seq[Int]): Int = {
    positions.size
  }

  def rank(documents: Map[Int, Seq[Int]]): Seq[Int] = {
    documents.toSeq.sortBy { doc: (Int, Seq[Int]) =>
      -score(doc._2)
    }.map { doc: (Int, Seq[Int]) => doc._1 }
  }

  def rank(documents: Seq[Map[Int, Seq[Int]]]): Seq[Int] = {
    val docIds: Seq[Set[Int]] = documents.map { _.keys.toSet }

    // full intersection set
    val allIntersect = docIds.reduceLeft(_ & _)

    // get pairwise intersections
    val pairIntersects: Seq[Set[Int]] = for {
      doc1 <- docIds;
      doc2 <- docIds
    } yield doc1 & doc2

    allIntersect.toSeq
  }

  def matchingDocs(term: Int, index: Index): Map[Int, Seq[Int]] = {
    index.getOrElse(term, Seq()).groupBy(_._1)
      .map { document: (Int, Seq[(Int, Int)]) =>
      (document._1, document._2.map(_._2))
    }
  }

  def search(query: String, index: Index): Seq[String] = {
    val terms = getTerms(query)
    val tokens = terms.map(_.hashCode)
    val documents = tokens.map { token => matchingDocs(token, index) }

    rank(documents).map { titleMap(_) }
  }

  def main(args: Array[String]) = {
    val index = loadIndex(indexFileName)

    println("title map size: " + titleMap.size)
    println("token map size: " + tokenMap.size)

    for (query <- args) {
      println("Searching for " + query)
      val ids = search(query, index)
      for (id <- ids) {
        println(id)
      }
    }
  }
}