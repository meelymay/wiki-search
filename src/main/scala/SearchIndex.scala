import collection.mutable.HashMap
import java.io._
import scala.io.{BufferedSource, Source}
import scala.util.Marshal
import scala.xml.pull._
import scala.xml._

trait SearchIndex extends Index {
  import Index._

  /**
   * Parse terms from query string.
   * TODO make utility method, so similar to getTerms
   */
  def parseQuery(text: String): Seq[String] = {
    // TODO stem the terms here?
    text.split("\\s+")
      .map(_.toLowerCase.filter(Character.isLetter(_)))
      .filter(!stop.contains(_))
  }

  /**
   * Calculate a simple tf-idf.
   */
  def tfIdf(doc: Seq[Int], numDocs: Int): Double = {
    // TODO should use frequency of term instead of count in document
    val tf: Double = doc.size
    val idf: Double = Math.log(numDocs/titleMap.size)
    tf/idf
  }

  /**
   * Score a single document's positions list.
   */
  def score(positions: Seq[Int]): Int = {
    positions.size
  }

  /**
   * Score a pair of documents' positions lists.
   */
  def score(positions: Seq[Int], positions2: Seq[Int]): Int = {
    0
  }

  /**
   * Order the set of article ids by score, descending.
   */
  def rank(documents: Map[Int, Seq[Int]]): Seq[Int] = {
    val numDocs = documents.size
    documents.toSeq.sortBy { doc: (Int, Seq[Int]) =>
      -tfIdf(doc._2, numDocs) // score(doc._2)
    }.map { doc: (Int, Seq[Int]) => doc._1 }
  }

  /**
   * Order the set of articles from the articles that matched each term in the query.
   */
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

  /**
   * Get all documents and positions lists for a token in the index.
   */
  def matchingDocs(token: Int, index: DocIndex): Map[Int, Seq[Int]] = {
    index.getOrElse(token, Seq()).groupBy(_._1)
      .map { document: (Int, Seq[(Int, Int)]) =>
      (document._1, document._2.map(_._2))
    }
  }

  /**
   * Find the documents that match a query and rank them.
   */
  def search(query: String): Seq[String] = {
    val terms = parseQuery(query)
    val tokens = terms.map(_.hashCode)
    val documents = tokens.map { token => matchingDocs(token, index) }

    rank(documents).map { titleMap(_) }
  }
}

object SearchMain {
  def main(args: Array[String]) = {
    // deserialize the index
    // "wikipedia_index.out", "wikipedia_titles.out", "wikipedia_tokens.out"
    val wikipedia = new Index with SearchIndex with SerializeIndex
    wikipedia.deserialize()

    println("title map size: " + wikipedia.titleMap.size)
    println("token map size: " + wikipedia.tokenMap.size)

    for (query <- args) {
      println("Searching for " + query)
      val ids = wikipedia.search(query)
      for (id <- ids) {
        println(id)
      }
    }
  }
}