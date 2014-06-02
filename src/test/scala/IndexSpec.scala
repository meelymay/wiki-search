import collection.mutable.HashMap
import org.scalatest._

class IndexSpec extends FlatSpec with WikiIndex {

  val shortDocumentTextIndex: Index = new HashMap()
  shortDocumentTextIndex.put(3556653, List((1,2)))
  shortDocumentTextIndex.put(109413500, List((1,0)))
  shortDocumentTextIndex.put(861720859, List((1,1)))
  val shortDocumentText: String = "short document text"

  "indexText" should "create a single articles index" in {
    val index = new WikipediaIndex()
    val id = 1

    val pageIndex = index.indexText(shortDocumentText, id)

    val expIndex = shortDocumentTextIndex
	val expTokens = Map(3556653 -> "text",
			 			109413500 -> "short",
			 			861720859 -> "document")

    assert(pageIndex == expIndex)
    assert(index.tokenMap == expTokens)
  }

  it should "add multiple positions" in {
    val index = new WikipediaIndex()
    val id = 1
    val text = shortDocumentText + " document short"

    val pageIndex = index.indexText(text, id)

    val expIndex = Map(3556653 -> List((1,2)),
    				   109413500 -> List((1,0), (1,4)),
    				   861720859 -> List((1,1), (1,3)))
	val expTokens = Map(3556653 -> "text",
			 			109413500 -> "short",
			 			861720859 -> "document")

    assert(pageIndex == expIndex)
    assert(index.tokenMap == expTokens)
  }

  it should "normalize capitalization" in {
  	val index = new WikipediaIndex()
    val id = 1
    val text = "Short DocumEnt teXt"

    val pageIndex: Index = index.indexText(text, id)

    assert(pageIndex == shortDocumentTextIndex)
  }

  "combineIndices" should "set a new index" in {
    val index = new WikipediaIndex()
    val id = 1

    val pageIndex = index.indexText(shortDocumentText, id)
    index.combineIndices(pageIndex)

    val expIndex = shortDocumentTextIndex

    assert(index.index == expIndex)
  }

  it should "add a new document to the index" in {
    val index = new WikipediaIndex()
    val id = 1

    val pageIndex = index.indexText(shortDocumentText, id)
    index.combineIndices(pageIndex)

    val id2 = 2
    // other is a stop word
    val text2 = "document other long short"
	val pageIndex2 = index.indexText(text2, id2)
    index.combineIndices(pageIndex2)

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

	val index = new WikipediaIndex()

	index.addPageToIndex(page)

	val expIndex = shortDocumentTextIndex
	val expTitles = Map(1 -> title)

	assert(index.index == expIndex)
	assert(index.titleMap == expTitles)
  }
}