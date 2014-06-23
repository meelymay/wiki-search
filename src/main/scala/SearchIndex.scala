import collection.mutable.HashMap
import java.io._
import scala.io.{BufferedSource, Source}
import scala.util.Marshal
import scala.xml.pull._
import scala.xml._

trait SearchIndex extends Index {
  import Index._

  type Positions = Set[Position]

  /**
   * Calculate a simple tf-idf.
   */
  def tfIdf(doc: Positions, numDocs: Int, docSize: Int): Double = {
    val tf: Double = doc.size/docSize.toDouble
    val idf: Double = Math.log(titleMap.size.toDouble/numDocs)
    tf*idf
  }

  /**
   * Order the set of article ids by score, descending.
   */
  def rank(documents: Map[DocId, Positions]): Seq[DocId] = {
    val numDocs = documents.size
    documents.toSeq.sortBy { case (doc, positions) =>
      -tfIdf(positions, numDocs, docSizeMap(doc))
    }.map { case (doc, positions) => doc }
  }

  /**
   * Order the set of articles from the articles that matched each term in the query.
   */
  def rankMultiple(documents: Map[Token, Map[DocId, Positions]]): Seq[DocId] = {
    val docIds: Map[Int, Set[Int]] = documents.map { case (token, docs) =>
      (token, docs.keys.toSet)
    }.toMap

    // get the per term scores of all documents
    val docScores: Seq[(DocId, Double)] = documents.toSeq
      .flatMap { case (token, docs) =>
        docs.toSeq.map { case (id, pos) =>
          val score = tfIdf(pos, docs.size, docSizeMap(id))
          (id, score)
        }
      }

    // sum scores of documents and sort
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
  def matchingExactDocs(tokens: Seq[Token]): Map[DocId, Positions] = {
    val documents: Seq[Map[DocId, Positions]] = tokens.map { token => matchingDocs(token) }
    val docIds: Seq[Set[DocId]] = documents.map { _.keys.toSet }

    // full intersection set
    val allIntersect: Set[DocId] = docIds.reduceLeft(_ & _)
    // remove non-all-intersect documents from each token's document map
    val containingDocs: Seq[Map[DocId, Positions]] = documents.map {
      docMap: Map[DocId, Positions] => docMap.filter { case (docId, pos) =>
        docIds.contains(docId)
      }
    }

    // now all maps in the sequence should contain the same documents
    // so for only the first token's document set
    // check that there is at least one position
    // such that all subsequent documents contain the correct subsequent positions
    containingDocs(0).flatMap { case (docId, positions) =>
      val pos: Positions = positions.filter { position: Position =>
        // check that the subsequent position exists in the subsequent document
        val consecutiveDocs = for (docIndex <- 1 to containingDocs.size
          if (containingDocs(docIndex)(docId).contains(position + docIndex)) ) yield true
        consecutiveDocs.size == containingDocs.size
      }
      if (!pos.isEmpty) {
        Seq((docId, pos))
      } else { Seq[(DocId, Positions)]() }
    }
  }

  /**
   * Get all documents and positions lists for a token in the index.
   */
  def matchingDocs(token: Token): Map[DocId, Positions] = {
    index.getOrElse(token, Seq()).groupBy { case (doc, positions) =>
      doc
    }.map { case (docId, documents) =>
      (docId,
       documents.map { case (id, position) =>
         position
       }.toSet)
    }
  }

  def matchingDocs(term: String): Map[DocId, Positions] = {
    if (term.contains(" ")) {
      matchingExactDocs(getTerms(term).map(_.hashCode))
    } else {
      matchingDocs(term.hashCode)
    }
  }

  def parseQuery(query: String): Seq[String] = {
    val terms = query.split("\\s+").toSeq
    var last = 0
    for (i <- 0 to terms.size-1
            if (terms(i)(terms(i).size-1) != '\\')) yield {
      val x: Seq[String] = terms.slice(last, i).map {s: String => s.slice(0, s.size-1) }.toSeq ++ Seq(terms(i))
      last = i + 1
      x.map(cleanTerm(_)).mkString(" ")
    }
  }

  /**
   * Find the documents that match a query and rank them.
   */
  def search(query: String): Seq[String] = {
    val terms = parseQuery(query)

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

    val query = args.mkString(" ")
    println("Searching for " + query)
    val ids = wikipedia.search(query)
    for (id <- ids) {
      println(id)
    }
  }
}