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
}