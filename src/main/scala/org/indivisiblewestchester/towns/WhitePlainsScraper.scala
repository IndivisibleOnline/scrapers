package org.indivisiblewestchester.towns

import scala.collection.JavaConversions._

import java.io.BufferedWriter
import java.io.FileOutputStream
import java.io.OutputStreamWriter

import java.time.format.DateTimeFormatter
import java.time.LocalDateTime

import org.indivisiblewestchester.Util

object WhitePlainsScraper {
  val baseUrl = "http://www.cityofwhiteplains.com"
  val startUrl = baseUrl + "/calendar.aspx"

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

    val dateStr = meeting.select("[class=date] span").filter(
            x => !x.text.contains("@") && x.text.contains("-") ).map(_.text).mkString
    val endSrcStr = dateStr.dropWhile(_ != '-').dropWhile(
        x => x.isWhitespace || x == '-').trim
    val endMap = if (endSrcStr.size < 6) {
        Map()
      } else {
        val startDate = start.takeWhile(!_.toUpper.equals('T'))
        val endDateTime =  startDate + " " + endSrcStr
        val parseDateFormat1 =  "uuuuMMdd h:m a"
        val parseFormatter1 = DateTimeFormatter.ofPattern(parseDateFormat1)
        val parseDateFormat2 =  "MMMM d, uuuu  h:m a"
        val parseFormatter2 = DateTimeFormatter.ofPattern(parseDateFormat2)
        val outFormatter = DateTimeFormatter.ofPattern("yyyyMMdd'T'HHmmss")
        try {
          Map( "DTEND" -> outFormatter.format(parseFormatter1.parse(endDateTime)) )
        } catch {
          case _ : Throwable => {
            try {
              Map( "DTEND" -> outFormatter.format(parseFormatter2.parse(endSrcStr)) )
            } catch {
              case _ : Throwable => Map()
            }
          }
        }
      }

    val eventDetailsLinks = meeting.select("a[onclick^=eventDetails]")
    val eidPattern = """.*eventDetails\(\s*(\d+)\s*,.*""".r
    val onclickAttrs = eventDetailsLinks.view.map(_.attr("onclick"))
    val eid = onclickAttrs.map(_  match {
        case eidPattern(eid) => Some(eid)
        case _ => None
      }).filter( _  != None).map(_.get).head

    endMap ++ Map[String,String](
      "DTSTART" -> start,
      "SUMMARY" -> ("White Plains: " + summary),
      "UID" -> (Util.despace(start) + "-" + Util.despace(summary)),
      "LOCATION" -> meeting.select("span[itemprop=location]").map(
                            _.select("*")).map(_.map(_.textNodes())).flatten.flatten.map(
                         _.toString).filter(_.trim.size != 0).mkString(", "),   
      "DESCRIPTION" -> meeting.select("p[itemprop=description]").text,
      "CATEGORIES" -> "WhitePlainsCalendar",
      "URL" -> (startUrl+"?EID="+eid)
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

    val startDoc = Util.urlToDoc(startUrl)
    val numMonths = 3

    val yearMonthHrefPat = """.*&year=(\d+)&month=(\d+)&.*""".r
    val yearMonthOnClickPat = (""".*eventDetails\((\d+)\s*,\s*(\d+)\s*,\s*(\d+)\s*,"""
                                + """\s*(\d+)\s*,\s*(\d+)\s*\s*\).*""").r

    val (startYearStr, startMonthStr) =
      (startDoc.select("table[summary=Calendar Display]").select("a")
        ).get(0).attr("href") match {
          case yearMonthHrefPat(year, month) => (year, month)
          case _ => {
            (startDoc.select("table[summary=Calendar Display]").select("a")
              ).get(0).attr("onclick") match {
                case yearMonthOnClickPat(eid, day, month, year, unk) => (year, month)
              }
          }
    }

    val monthYears = getNMonths(
            (startMonthStr.toInt, startYearStr.toInt), numMonths)

    val monthDocs = monthYears.map( (my:Tuple2[Int, Int]) => 
      (startUrl+"?view=list&year="+my._2
        +"&month="+my._1)).view.map(Util.urlToDoc(_))

    val meetings = monthDocs.view.map(docToMeetingList(_))
 
    val out_writer =
      new BufferedWriter(new OutputStreamWriter(new FileOutputStream(iCalOut)))

    out_writer.write(Util.iCalHeader("White Plains Calendar"))

    meetings.flatten.map(meetingToMap _ ).map(
                                     Util.mapToIcal _ ).foreach(out_writer.write _ )

    out_writer.write(Util.iCalFooter)

    out_writer.close()
  }
}