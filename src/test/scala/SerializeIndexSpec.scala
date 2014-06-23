import collection.mutable.HashMap
import collection.mutable.Stack
import org.scalatest._

class SerializeIndexSpec extends FlatSpec {
  import Index._
  val index = new Index with SerializeIndex

  "entryToString" should "concat to string with delim" in {
	val ie = (12, 34)
	val s = index.entryToString(ie)
	val expS = "12:34"
	assert(s == expS)
  }

  "parseEntries" should "parse ints from string" in {
	val s = "1001:2002"
	val ie = index.parseEntries(s)
	val expIe = Seq((1001, 2002))
	assert(ie == expIe)
  }

  it should "parse multiple int pairs from string" in {
	val s = "1001:2002,1:2,3:4"
	val ie = index.parseEntries(s)
	val expIe = Seq((1001, 2002),(1,2),(3,4))
	assert(ie == expIe)
  }

  it should "parse (0,0) for malformed string" in {
	val s = "1001,2002,55:66"
	val ie = index.parseEntries(s)
	val expIe = Seq((0,0), (0,0), (55,66))
	assert(ie == expIe)
  }

  "idNameToString" should "concat id and name" in {
  	val ie = (12, "This is a Title")
	val s = index.idNameToString(ie)
	val expS = "12:This is a Title"
	assert(s == expS)
  }

  "parseIdString" should "separate int and name" in {
  	val s = "1001:A Title Article"
	val ie = index.parseIdString(s)
	val expIe = (1001, "A Title Article")
	assert(ie == expIe)
  }

  it should "pase 0, empty string if malformed" in {
  	val s = "1001 A Title Article"
	val ie = index.parseIdString(s)
	val expIe = (0, "0")
	assert(ie == expIe)
  }

  "serialize map" should "write and read to file" in {
	val index = new Index with SerializeIndex
	val m = Seq((1, "Title Twenty"),
				(345, "Eternal Sunshine"),
				(987, "Adaptation Best Ever"))
	// TODO these should be tmp files
	val filename = "src/test/resources/test_map"
	index.dumpMap(m.toMap, filename)
	val readMap = index.loadIdMap(filename)
	assert(m == readMap)
  }

  "deserialize size map" should "write and read to file" in {
	val index = new Index with SerializeIndex
	val m = Seq((1, "50"),
				(345, "1000"),
				(987, "999"))
	val expSizes = m.map { case (id, count) => (id, count.toInt) }.toMap
	// TODO these should be tmp files
	val filename = "src/test/resources/test_size_map"
	index.dumpMap(m.toMap, filename)
	index.loadDocSizeMap(filename)
	assert(expSizes == index.docSizeMap)
  }
}