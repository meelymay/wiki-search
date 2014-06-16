import collection.mutable.HashMap
import java.io._
import scala.io.{BufferedSource, Source}
import scala.util.Marshal
import scala.xml.pull._
import scala.xml._

trait SearchIndex extends Index {
  import Index._

  /**
   * Calculate a simple tf-idf.
   */
  def tfIdf(doc: Seq[Int], numDocs: Int): Double = {
    // TODO should use frequency of term instead of count in document
    val tf: Double = doc.size
    val idf: Double = Math.log(titleMap.size.toDouble/numDocs)
    tf/idf
  }

  /**
   * Score a single document's positions list.
   */
  def score(positions: Seq[Int]): Double = {
    positions.size
  }

  /**
   * Score a pair of documents' positions lists.
   */
  def score(positions: Seq[Int], positions2: Seq[Int]): Double = {
    0
  }

  /**
   * Score a set of documents' positions lists.
   */
  def scoreMultiple(positions: Seq[Seq[Position]]): Double = {
    0
  }

  /**
   * Order the set of article ids by score, descending.
   */
  def rank(documents: Map[DocId, Seq[Position]]): Seq[DocId] = {
    val numDocs = documents.size
    documents.toSeq.sortBy { doc: (Int, Seq[Int]) =>
      -tfIdf(doc._2, numDocs)
    }.map { doc: (Int, Seq[Int]) => doc._1 }
  }

  /**
   * Order the set of articles from the articles that matched each term in the query.
   *
   * documents -> positions for each term
   */
  def rankMultiple(documents: Map[Token, Map[DocId, Seq[Position]]]): Seq[DocId] = {
    val docIds: Map[Int, Set[Int]] = documents.map { case (token, docs) =>
      (token, docs.keys.toSet)
    }.toMap

    // full intersection set
    /* val allIntersect = docIds.reduceLeft(_ & _)

    // get pairwise intersections
    val pairIntersects: Seq[Set[Int]] = for {
      doc1 <- docIds;
      doc2 <- docIds
    } yield doc1 & doc2 */

    val docScores: Seq[(DocId, Double)] = documents.toSeq.flatMap { case (token, docs) =>
      docs.toSeq.map { case (id, pos) =>
        (id, score(pos))
      }
    }

    docScores.groupBy { _._1 }.map { case (id, scores) =>
      val score = scores.map { _._2 }.sum
      (id, score)
    }.toSeq.sortBy { -_._2 }.map { _._1 }
  }

  /**
   * Get all documents and positions lists for a token in the index.
   */
  def matchingDocs(token: Int): Map[Int, Seq[Int]] = {
    index.getOrElse(token, Seq()).groupBy(_._1)
      .map { document: (Int, Seq[(Int, Int)]) =>
      (document._1, document._2.map(_._2))
    }
  }

  def matchingDocs(term: String): Map[Int, Seq[Int]] = {
    matchingDocs(term.hashCode)
  }

  /**
   * Find the documents that match a query and rank them.
   */
  def search(query: String): Seq[String] = {
    val terms = getTerms(query)
    val tokens = terms.map(_.hashCode)
    val documents = tokens.map { token => (token, matchingDocs(token)) }.toMap

    rankMultiple(documents).map { titleMap(_) }
  }
}

object SearchMain {
  def main(args: Array[String]) = {
    // deserialize the index
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