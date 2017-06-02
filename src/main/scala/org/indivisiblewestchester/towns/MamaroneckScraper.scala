package org.indivisiblewestchester.towns

import scala.collection.JavaConversions._

import java.io.BufferedWriter
import java.io.FileOutputStream
import java.io.OutputStreamWriter

import org.indivisiblewestchester.Util

object MamaroneckScraper {

  def intNextMonthYear(month: Int, year: Int) = 
      (((month + 1) % 12), ((month + 1)/12) + year)

  def nextMonthYear(month: Int, year: Int) = {
    val (m, y) = intNextMonthYear(month - 1, year)
    ((m+1), y)
  }

  def docToMeetingList(doc: org.jsoup.nodes.Document) = {
    doc.select("div[class=calendars]").select("li")
  }

  def stripDatePunc(in: String) =
      in.replaceAll("-","").replaceAll(":","")

  def meetingToMap(meeting: org.jsoup.nodes.Element) = {
    val summary = meeting.select("a[id^=eventTitle]").text
    val start = stripDatePunc(meeting.select("span[itemprop=startDate]").text)
    val eventIdStr = meeting.select("a[id^=calendarEvent]").get(0).id
    val eventIdPat = "^calendarEvent([0-9]+)$".r
    val eventURLMap = 
      eventIdStr match {
        case eventIdPat(id) => Map("URL" ->
	  ("http://www.townofmamaroneck.org/Calendar.aspx?EID="+id))
	case _ => Map()
    }

    eventURLMap ++ Map[String,String](
      "DTSTART" -> start,
      "SUMMARY" -> summary,
      "UID" -> (Util.despace(start) + "-" + Util.despace(summary)),
      "LOCATION" -> meeting.select("span[itemprop=location]").map(
      		      _.select("*")).map(_.map(_.textNodes())).flatten.flatten.map(
		         _.toString).filter(_.trim.size != 0).mkString(", "),   
      "DESCRIPTION" -> meeting.select("p[itemprop=description]").text,
      "CATEGORIES" -> "MamaroneckCalendar"
    )
  }

  def getNMonths(yearMonth: Tuple2[Int, Int], num: Int): 
    List[Tuple2[Int, Int]] =
    if (num < 1) {
      List[Tuple2[Int, Int]]()
    } else {
      val (year, month) = yearMonth
      yearMonth +: (getNMonths(nextMonthYear(year, month), num - 1))
    }

  def main(args: Array[String]) = {
    val iCalOut = args(0)

    val baseUrl = "http://www.townofmamaroneck.org/calendar.aspx"
    val startDoc = Util.urlToDoc(baseUrl)
    val numMonths = 3

    val yearMonthPat = """.*&year=(\d+)&month=(\d+)&.*""".r
    val (startYearStr, startMonthStr) =
      (startDoc.select("table[summary=Calendar Display]").select("a")
        ).get(0).attr("href") match {
          case yearMonthPat(year, month) => (year, month)
    }
    val monthYears = getNMonths(
    	(startMonthStr.toInt, startYearStr.toInt), numMonths)

    val monthDocs = monthYears.map( (my:Tuple2[Int, Int]) => 
      ("http://www.townofmamaroneck.org/calendar.aspx?view=list&year="+my._2
	+"&month="+my._1)).view.map(Util.urlToDoc(_))

    val meetings = monthDocs.view.map(docToMeetingList(_))
 
    val out_writer =
      new BufferedWriter(new OutputStreamWriter(new FileOutputStream(iCalOut)))

    out_writer.write(Util.iCalHeader("Mamaroneck Calendar"))

    meetings.flatten.map(meetingToMap _ ).map(
    				 Util.mapToIcal _ ).foreach(out_writer.write _ )

    out_writer.write(Util.iCalFooter)

    out_writer.close()
  }
}