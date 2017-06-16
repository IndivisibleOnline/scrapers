package org.indivisiblewestchester.towns

import org.indivisiblewestchester.Util

import scala.collection.JavaConversions._

import java.io.BufferedWriter
import java.io.OutputStreamWriter
import java.io.FileOutputStream

object YonkasScraper {
  val baseUrl = "http://www.yonkersny.gov"
  val startUrl = "http://www.yonkersny.gov/live/city-calendar"

  val VeventStartPat = """(?s)(^.*)(BEGIN:VEVENT\s+)""".r
  val VeventEndPat = """(?s)(END:VEVENT\s+)(.*$)""".r

  def nextMonth(doc: org.jsoup.nodes.Document) =
    baseUrl + doc.select("a.next").attr("href")

  def getMonths(url: String, numMonths: Int): Seq[org.jsoup.nodes.Document] =
    if (numMonths > 0) {
       val doc = org.jsoup.Jsoup.connect(url).get()
       doc +: getMonths(nextMonth(doc), numMonths - 1)
    } else {
       Seq[org.jsoup.nodes.Document]()
    }

  def eventUrlToIcal(url: String): String = {
    val eventDoc= org.jsoup.Jsoup.connect(url).get();
    val iCalLink = eventDoc.select("a.outlookcalendar");
    if (iCalLink.size != 1)
       throw new IllegalStateException("oops " + url + " generated "
       	     + iCalLink.size + " ical links!")
    else {
      //var x = org.jsoup.Jsoup.connect(baseUrl
	//	+ iCalLink.first.attr("href")).get()
	//"([A-Z]+:)".r replaceAllIn(x.select("body").text, "\r\n$1");
	scala.io.Source.fromURL(baseUrl+iCalLink.first.attr("href")).mkString
    }
  }

  def stripBeforeVevent(evs: Seq[String]) =
    evs.map( VeventStartPat replaceAllIn(_, "$2"))

  def stripAfterVevent(evs: Seq[String]) =
    evs.map( VeventEndPat replaceAllIn(_, "$1"))

  val categoryPat = """(?s)^(.*\n)?(CATEGORIES:\s*)(.*$)""".r

  def insertCategory(evStr: String) = {
    if (categoryPat.findFirstIn(evStr) != None) {
      categoryPat.replaceAllIn(evStr, _  match {
	  case categoryPat(begin, cat, rest) => 
	    (begin + cat + "YonkersCalendar, " + rest)
	case _ => throw new IllegalStateException("evStr = "+evStr)
	  } )
    } else {
      VeventEndPat.replaceAllIn(evStr, _ match {
	case VeventEndPat(end, rest) =>
                     ("CATEGORIES: YonkersCalendar\n" + end + rest)
	case _ => throw new IllegalStateException("evStr = "+evStr)
	  } )
    } 
  }	

  val summaryPat = """(?s).*SUMMARY:\s*([^\r\n]*).*?""".r
  val startDatePat = """(?s).*DTSTART:\s*([^\r\n]*).*?""".r

  def insertUID(evStr: String) = {
    val summary = evStr match {
      case summaryPat(summary) => summary.trim
      case _ => throw new IllegalStateException("evStr = "+evStr)
    }
    val startDate = evStr match {
      case startDatePat(start) => start.trim
      case _ => throw new IllegalStateException("evStr = "+evStr)
    }
    val uidStr = "UID:" + Util.despace(startDate) + "-" + Util.despace(summary)
    VeventEndPat.replaceAllIn(evStr, _ match {
      case VeventEndPat(end, rest) =>
                     (uidStr + "\n" + end + rest)
      case _ => throw new IllegalStateException("evStr = "+evStr)
    } )
  }

  def main( args:Array[String] ): Unit = {
    val iCalOut = args(0)

    val numMonths = 3

    val calPages = getMonths(startUrl, numMonths)
    val eventLinks = calPages.map(_.select("a.calendar_eventlink"))
    val eventUrls = eventLinks.flatten.map(baseUrl + _.attr("href"))

    val fullIcals = eventUrls.map(eventUrlToIcal)

    val headlessIcals = fullIcals.head +: stripBeforeVevent(fullIcals.tail)
    val fixedIcals = stripAfterVevent(headlessIcals.init) :+ headlessIcals.last

    val catIcals = fixedIcals.map(insertCategory)

    val uidIcals = catIcals.map(insertUID)
    val finalIcals = uidIcals.map(
       """(?s)(.*SUMMARY:)(.*)""".r replaceAllIn(_, "$1Yonkers: $2"))

    val out_writer =
      new BufferedWriter(new OutputStreamWriter(new FileOutputStream(iCalOut)))

    finalIcals map(out_writer.write(_))

    out_writer.close()
  }
}



