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

  "idNameToString" should "" in {

  }

  "parseIdString" should "" in {

  }
}