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
  def tfIdf(doc: Seq[Position], numDocs: Int): Double = {
    // TODO should use frequency of term instead of count in document
    val tf: Double = doc.size
    val idf: Double = Math.log(titleMap.size.toDouble/numDocs)
    tf/idf
  }

  /**
   * Score a single document's positions list.
   */
  def score(positions: Seq[Position]): Double = {
    positions.size
  }

  /**
   * Score a pair of documents' positions lists.
   */
  def score(positions: Seq[Position], positions2: Seq[Position]): Double = {
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

    val docScores: Seq[(DocId, Double)] = documents.toSeq
      .flatMap { case (token, docs) =>
        docs.toSeq.map { case (id, pos) =>
          (id, score(pos))
        }
      }

    docScores.groupBy { case (id, score) => id }
      .map { case (id, scores) =>
        (id, scores.map { case (doc, score) => score }.sum)
      }.toSeq
      .sortBy { case (id, score) => -score }
      .map { case (id, score) => id }
  }

  /**
   * Get all documents and positions lists for a token in the index.
   */
  def matchingDocs(token: Token): Map[DocId, Seq[Position]] = {
    index.getOrElse(token, Seq()).groupBy { case (doc, positions) =>
      doc
    }.map { case (docId, documents) =>
      (docId, documents.map { case (id, position) =>
        position
      })
    }
  }

  def matchingDocs(term: String): Map[DocId, Seq[Position]] = {
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