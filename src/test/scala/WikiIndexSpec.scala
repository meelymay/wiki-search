import collection.mutable.HashMap
import collection.mutable.Stack
import org.scalatest._

class WikiIndexSpec extends FlatSpec with WikiIndex {

  "getTerms" should "split the terms" in {
  	val termStr = "amelia skye"
    val terms = getTerms(termStr)
    assert(terms(0) == "amelia")
    assert(terms(1) == "skye")
  }

  it should "remove non-letter characters" in {
  	val termStr = "a.m'elia,"
    val terms = getTerms(termStr)
    assert(terms(0) == "amelia")
  }

  it should "remove stop words" in {
  	val termStr = "amelia is the craziest"
    val terms = getTerms(termStr)
    assert(terms(0) == "amelia")
    assert(terms(1) == "craziest")
  }

  "entryToString" should "concat to string with delim" in {
	val ie = (12, 34)
	val s = entryToString(ie)
	val expS = "12:34"
	assert(s == expS)
  }

  "parseEntries" should "parse ints from string" in {
	val s = "1001:2002"
	val ie = parseEntries(s)
	val expIe = Seq((1001, 2002))
	assert(ie == expIe)
  }

  it should "parse multiple int pairs from string" in {
	val s = "1001:2002,1:2,3:4"
	val ie = parseEntries(s)
	val expIe = Seq((1001, 2002),(1,2),(3,4))
	assert(ie == expIe)
  }

  it should "parse (0,0) for malformed string" in {
	val s = "1001,2002,55:66"
	val ie = parseEntries(s)
	val expIe = Seq((0,0), (0,0), (55,66))
	assert(ie == expIe)
  }

  "idNameToString" should "concat id and name" in {
  	val ie = (12, "This is a Title")
	val s = idNameToString(ie)
	val expS = "12:This is a Title"
	assert(s == expS)
  }

  "parseIdString" should "separate int and name" in {
  	val s = "1001:A Title Article"
	val ie = parseIdString(s)
	val expIe = (1001, "A Title Article")
	assert(ie == expIe)
  }

  it should "pase 0, empty string if malformed" in {
  	val s = "1001 A Title Article"
	val ie = parseIdString(s)
	val expIe = (0, "")
	assert(ie == expIe)
  }

  "serialize map" should "write and read to file" in {
	val m = Map(1 -> "Title Twenty",
				345 -> "Eternal Sunshine",
				987 -> "Adaptation Best Ever")
	val filename = "test_map"
	dumpMap(m, filename)
	val readMap = loadIdMap(filename)
	assert(m == readMap)
  }

  "serialize index" should "write and read to file" in {
	val index = HashMap(1 -> Seq((1,2), (3,4), (5,6)),
						345 -> Seq((987,876), (10000, 20000)),
						987 -> Seq((11,22), (33,44), (55,66), (33,55)))
	val filename = "test_map"
	dumpIndex(index, filename)
	val readIndex = loadIndex(filename)
	assert(index == readIndex)
  }
}