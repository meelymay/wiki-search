import scala.io.Source
import scala.xml._

object Search {
  def main(args: Array[String]) = { 
    val anarchism = "src/main/resources/anarchism.xml"
    println(anarchism)
    val source = XML.loadFile(anarchism)
    println(source.text)
  }
}
