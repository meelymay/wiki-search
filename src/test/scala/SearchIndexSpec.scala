import org.scalatest._

class SearchIndexSpec extends FlatSpec {

  "parseQuery" should "find compound terms" in {
	val s = "sound\\ cloud is cool"
	val index = createIndex(Seq())
	assert(index.parseQuery(s) == Seq("sound cloud", "is", "cool"))
  }

  "tfidf" should "compute freq over inverse doc freq" in {
  	val docs = Seq("dog words bone park walk",
  				   "cats walk jump land milk sleep",
  				   "people words milk walk park sleep")
	val index = createIndex(docs)

	val doc = Set(1,2,3,4)
	val numDocs = 2
	val docSize = 5
    val tfidf = index.tfIdf(doc, numDocs, docSize)
    val expTfIdf = (4/5.0)*Math.log(3.0/numDocs)
    assert(tfidf == expTfIdf)
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
  	val docs = Seq("cat dog other",
  				   "dog cat fish",
  				   "fish cat cow moo",
  				   "cow", "a", "b", "c")
  	val index = createIndex(docs)
	val matching = Seq(index.matchingDocs("cat"),
					   index.matchingDocs("dog"),
					   index.matchingDocs("fish"))
	val ranked = index.rankMultiple(matching)
	val expRanked = Seq(1,0,2)

	assert(ranked == expRanked)
  }

  "matchingExactDocs" should "get docs that match subsequent terms" in {
	val docs = Seq("cat cat dog cat cat",
				   "dog cat cat",
				   "fish cat dog cat dog",
				   "cow")
	val index = createIndex(docs)
	val matching = index.matchingDocs("cat\\ dog")
	val expMatching = Map(0 -> Set(1),
						  2 -> Set(1, 3))

	assert(matching == expMatching)
  }

  "matchingDocs" should "get only docs with the term" in {
	val docs = Seq("cat cat cat cat",
				   "dog cat cat",
				   "fish cat",
				   "cow")
  	val index = createIndex(docs)
  	val matching = index.matchingDocs("cat")
	val expMatching = Map(0 -> Set(0,1,2,3),
						  1 -> Set(1,2),
						  2 -> Set(1))

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

  it should "rank the doc with all terms highest" in {
	val docs = Seq("cat dog other",
				   "dog cat fish",
				   "fish cat cow moo",
				   "cow")
	val index = createIndex(docs)
	val ranked = index.search("cat dog fish")
	val expRanked = Seq("dog","cat","fish")

	assert(ranked == expRanked)
  }

  it should "find single documents" in {
	val docs = Seq("cat dog other",
				   "dog cat fish",
				   "fish cat cow moo",
				   "cow")
	val index = createIndex(docs)
	val ranked = index.search("cow")
	val expRanked = Seq("cow", "fish")

	assert(ranked == expRanked)
  }

  it should "find multi-word terms" in {
	val docs = Seq("cat dog other",
				   "dog cat fish",
				   "fish cat cow pasture moo",
				   "cow")
	val index = createIndex(docs)
	val ranked = index.search("cow\\ pasture")
	val expRanked = Seq("fish")

	assert(ranked == expRanked)
  }

  it should "find fish" in {
	val docs = Seq("cat dog other",
				   "dog cat fish",
				   "fish cat cow moo",
				   "cow")
	val index = createIndex(docs)
	val ranked = index.search("fish")
	val expRanked = Seq("dog", "fish")

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