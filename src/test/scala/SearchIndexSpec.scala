import org.scalatest._

class SearchIndexSpec extends FlatSpec {

  "tfidf" should "compute freq over inverse doc freq" in {
  	val docs = Seq("dog words bone park walk",
  				   "cats walk jump land milk sleep",
  				   "people words milk walk park sleep")
	val index = createIndex(docs)

	val doc = Seq(1,2,3,4)
	val numDocs = 2
    val tfidf = index.tfIdf(doc, numDocs)
    val expTfIdf = 4/Math.log(3.0/numDocs)
    assert(tfidf == expTfIdf)
  }

  "score" should "count occurrences" in {
  	val index = createIndex(Seq())
  	val pos = Seq(1,2,3,4,5)
  	assert(index.score(pos) == 5)
  }

  "scorePair" should "" in {
  }

  "scoreMultiple" should "" in {
  }

  "rank" should "score more occurrences higher" in {
  	val docs = Seq("cat cat cat cat",
  				   "dog cat cat",
  				   "fish cat",
  				   "cow")
  	val index = createIndex(docs)
  	val matching = index.matchingDocs("cat")
	val ranked = index.rank(matching)
	val expRanked = Seq(0,1,2)

	assert(ranked == expRanked)
  }

  "rankMultiple" should "rank highest doc containing all" in {
  	val docs = Seq("cat dog",
  				   "dog cat fish",
  				   "fish cat",
  				   "cow")
  	val index = createIndex(docs)
	val matching = Map(98262   -> index.matchingDocs("cat"),
					   199644  -> index.matchingDocs("dog"),
					   3143256 -> index.matchingDocs("fish"))
	val ranked = index.rankMultiple(matching)
	val expRanked = Seq(1,2,0)

	assert(ranked == expRanked)
  }

  "matchingDocs" should "get only docs with the term" in {
	val docs = Seq("cat cat cat cat",
				   "dog cat cat",
				   "fish cat",
				   "cow")
  	val index = createIndex(docs)
  	val matching = index.matchingDocs("cat")
	val expMatching = Map(0 -> Seq(0,1,2,3),
						  1 -> Seq(1,2),
						  2 -> Seq(1))

  	assert(matching == expMatching)
  }

  "search" should "get docs relevant to cat" in {
	val docs = Seq("cat cat cat cat",
  				   "dog cat cat",
  				   "fish cat",
  				   "cow")
  	val index = createIndex(docs)

  	val results = index.search("cat")
	val expResults = Seq("cat", "dog", "fish")

  	assert(results.toSeq == expResults)
  }

  "search" should "rank the doc with all terms highest" in {
	val docs = Seq("cat dog",
				   "dog cat fish",
				   "fish cat cow",
				   "cow")
	val index = createIndex(docs)
	val ranked = index.search("cat dog fish")
	val expRanked = Seq("dog","fish","cat")

	assert(ranked == expRanked)
  }

  def createIndex(documents: Seq[String]): SearchIndex = {
	val index = new Index with SearchIndex with BuildIndex

	for ((text, id) <- documents.zipWithIndex) {
	  val page = (id, text.split(" ")(0), text)
	  index.addPageToIndex(page)
	}

	index
  }
}