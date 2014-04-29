import scala.io.Source
import scala.xml.pull._

object Main extends App {
  val xml = new XMLEventReader(Source.fromFile("test.xml"))

  def parse(xml: XMLEventReader) {
    def loop(currNode: List[String]) {
      if (xml.hasNext) {
        xml.next match {
          case EvElemStart(_, label, _, _) =>
            println("Start element: " + label)
            loop(label :: currNode)
          case EvElemEnd(_, label) =>
            println("End element: " + label)
            loop(currNode.tail)
          case EvText(text) =>
            println(currNode + ": " + text)
            loop(currNode)
          case _ => loop(currNode)
        }
      }
    }
    loop(List.empty)
  }

  def xmlLabelMatches(targetLabel: String)(event: XMLEvent): Boolean = {
    event match {
      case EvElemEnd(_, label) => {
        (label == targetLabel)
      }
      case _ => false
    }
  }

  def getXMLTextUntil(xmlReader: XMLEventReader, untilCondition: (XMLEvent) => Boolean): (String, Option[XMLEvent]) = {
    if (xmlReader.hasNext) {
      val event = xmlReader.next
      if (untilCondition(event)) ("", Some(event))
      else {
        val nextEvent = getXMLTextUntil(xmlReader, untilCondition)
        event match {
          case EvText(text) => (text + nextEvent._1, nextEvent._2)
          case _ => nextEvent
        }
      }
    } else {
      ("", None)
    }
  }

  def getNextPage(event: XMLEvent, eventReader: XMLEventReader): Page = {
    // println("xml event: " + event)
    event match {
      case EvElemStart(_, label, _, _) => {
        // println("start: " + label)
        if (label == "id") {
          val id = getXMLTextUntil(eventReader, xmlLabelMatches("id")(_))._1.trim.toInt
          val rest = getNextPage(eventReader.next, eventReader)
          (id, rest._2)
        }
        if (label == "title") {
          val title = getXMLTextUntil(eventReader, xmlLabelMatches("title")(_))._1
          println(title)
          val rest = getNextPage(eventReader.next, eventReader)
          (rest._1, title + rest._2)
        } else {
          val rest = getNextPage(eventReader.next, eventReader)
          rest
        }
      }
      case EvText(text) => {
        // println("text: " + text)
        val (text, event) = getXMLTextUntil(eventReader, (event: XMLEvent) => 
          event match {
            case EvText(_) => false
            case _ => true
          })
        val rest = getNextPage(event.get, eventReader)
        (rest._1, text + rest._2)
      }
      case EvElemEnd(_, label) => { 
        if (label == "page") {
          println("finishing page")
          (0, "")
        } else {
          getNextPage(eventReader.next, eventReader)
        }
      }
      case _ => { getNextPage(eventReader.next, eventReader) }
    }
  }

  parse(xml)
}