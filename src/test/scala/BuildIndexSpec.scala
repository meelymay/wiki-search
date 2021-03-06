import collection.mutable.{HashMap, MutableList}
import org.scalatest._

class BuildIndexSpec extends FlatSpec {
  import Index._

  val shortDocumentTextIndex: DocIndex = new HashMap()
  shortDocumentTextIndex.put(3556653, MutableList((1,2)))
  shortDocumentTextIndex.put(109413500, MutableList((1,0)))
  shortDocumentTextIndex.put(861720859, MutableList((1,1)))
  val shortDocumentText: String = "short document text"

  "getTerms" should "split the terms" in {
    val index = new Index with BuildIndex

    val termStr = "amelia skye"
    val terms = getTerms(termStr)
    assert(terms(0) == "amelia")
    assert(terms(1) == "skye")
  }

  it should "remove non-letter characters" in {
    val index = new Index with BuildIndex

    val termStr = "a.m'elia,"
    val terms = getTerms(termStr)
    assert(terms(0) == "amelia")
  }

  it should "remove stop words" in {
    val index = new Index with BuildIndex

    val termStr = "amelia is the craziest"
    val terms = getTerms(termStr)
    assert(terms(0) == "amelia")
    assert(terms(1) == "craziest")
  }

  "indexText" should "create a single articles index" in {
    val index = new Index with BuildIndex
    val id = 1

    index.indexText(shortDocumentText, id)

    val expIndex = shortDocumentTextIndex
	  val expTokens = Map(3556653 -> "text",
			 			109413500 -> "short",
			 			861720859 -> "document")

    assert(index.index == expIndex)
    assert(index.tokenMap == expTokens)
  }

  it should "add multiple positions" in {
    val index = new Index with BuildIndex
    val id = 1
    val text = shortDocumentText + " document short"

    index.indexText(text, id)

    val expIndex = Map(3556653 -> List((1,2)),
    				   109413500 -> List((1,0), (1,4)),
    				   861720859 -> List((1,1), (1,3)))
	val expTokens = Map(3556653 -> "text",
			 			109413500 -> "short",
			 			861720859 -> "document")

    assert(index.index == expIndex)
    assert(index.tokenMap == expTokens)
  }

  it should "normalize capitalization" in {
    val index = new Index with BuildIndex
    val id = 1
    val text = "Short DocumEnt teXt"

    index.indexText(text, id)

    assert(index.index == shortDocumentTextIndex)
  }

  "combineIndices" should "set a new index" in {
    val index = new Index with BuildIndex
    val id = 1

    index.indexText(shortDocumentText, id)

    val expIndex = shortDocumentTextIndex

    assert(index.index == expIndex)
  }

  it should "add a new document to the index" in {
    val index = new Index with BuildIndex
    val id = 1

    index.indexText(shortDocumentText, id)

    val id2 = 2
    // other is a stop word
    val text2 = "document other long short"
    index.indexText(text2, id2)

    val expIndex = Map(3556653 -> List((1,2)),
    				   109413500 -> List((1,0), (2,2)),
    				   861720859 -> List((1,1), (2,0)),
    				   3327612 -> List((2,1)))
    assert(index.index == expIndex)
  }

  "addPageToIndex" should "add index and add title and tokens to maps" in {
    val id = 1
  	val title = "an article"
  	val text = shortDocumentText
	  val page = (id, title, text)

    val index = new Index with BuildIndex

	  index.addPageToIndex(page)

  	val expIndex = shortDocumentTextIndex
  	val expTitles = Map(1 -> title)

  	assert(index.index == expIndex)
  	assert(index.titleMap == expTitles)
  }
}