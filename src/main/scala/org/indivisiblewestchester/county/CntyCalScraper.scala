package org.indivisiblewestchester.county

import java.time.format.DateTimeFormatter
import java.time.LocalDateTime

import java.io.StringWriter
import java.io.BufferedWriter
import java.io.OutputStreamWriter
import java.io.FileOutputStream

import scala.collection.JavaConversions._

import org.indivisiblewestchester.Util

object CntyCalScraper {
  def stripCategory(evUrl: String) = {
    val evUrlPat = "^([^/]*//[^/]*/[^/]*/[^/]*/)(-|[0-9]+)(/[^/]*)$".r
      evUrl match {
        case evUrlPat(begin,cat,end) => begin + "-" + end
	     case _ => throw new IllegalStateException(evUrl)
	}
  }

  def reverse[A, B](m: Map[A, Set[B]]) =
    m.values.toSet.flatten.map(v => (v, m.keys.filter(m(_).contains(v)))).toMap

  def date1EltConv(date: String) = {
    val dateFormat =  "MMMM dd, uuuu"
        val parseFormatter = DateTimeFormatter.ofPattern(dateFormat)
	    val outFormatter = DateTimeFormatter.ofPattern("yyyyMMdd")

    outFormatter.format(parseFormatter.parse(date))
  }

  def date2EltConv(date: String, time: String, hourOffset: Int = 0) = {
    val dateFormat =  "MMMM dd, uuuu h:ma"
    val parseFormatter = DateTimeFormatter.ofPattern(dateFormat)
    val outFormatter = DateTimeFormatter.ofPattern("yyyyMMdd'T'HHmmss")

    outFormatter.format(
      LocalDateTime.parse(date + " " + time,
			  parseFormatter).plusHours(hourOffset))
  }

def eventPageToiCal(srcUrl: String,
                    eventDoc: org.jsoup.nodes.Document,
		    cats: Iterable[String]) =  {
    val buf = new StringWriter()

    buf.append("BEGIN:VEVENT\n")

    val name = eventDoc.select("td.contentheading").text
        buf.append("SUMMARY:Westchester: ").append(Util.tidyForIcal(name)).append("\n")

    val dateElts =
      eventDoc.select("span[style*=3d4699]").text.split("\\xa0").map(
        ("""(?i)^\s*(monday|tuesday|wednesday|thursday|"""
	 +"""friday|saturday|sunday)?(\s|,)*""").r
	  replaceAllIn(_, ""))

    val (start, end) = {
      if (dateElts(1).size > 0)
        if (dateElts.size > 2 && dateElts(2).size > 0)
          (date2EltConv(dateElts(0), dateElts(1)),
	   date2EltConv(dateElts(0), dateElts(2)))
      	else
	  (date2EltConv(dateElts(0), dateElts(1)),
	   date2EltConv(dateElts(0), dateElts(1), 1))
      else
        (date1EltConv(dateElts(0)), date1EltConv(dateElts(0)))
    }

    buf.append("DTSTART:").append(start).append("\n")
    buf.append("DTEND:").append(end).append("\n")

    val categories = cats.mkString(",")
    buf.append("CATEGORIES:").append(categories).append("\n")

    val website = srcUrl
    buf.append("URL:").append(srcUrl).append("\n")

    val description = eventDoc.select("tr[align=left]").text
    buf.append("DESCRIPTION:").append(Util.tidyForIcal(description)).append("\n")

    val locationLineNBSP =
      ("[Ll]ocation(\\s|[_:]|\\00a0|\u00a0)*".r replaceAllIn(
        eventDoc.select("td.ev_detail:matches(ocation)").text, "")).trim

    val locationLine = locationLineNBSP.replaceAll("\u00a0"," ").trim

    buf.append("LOCATION:").append(Util.tidyForIcal(locationLine)).append("\n")

    val organizerName = {
      val tmp = ("[Cc]ontact(\\s|[_:]|\u00a0)*".r replaceAllIn(
        eventDoc.select("td.ev_detail:matches(ontact)").text, "")).trim
      if (tmp.length == 0)
        "unknown"
      else
        tmp
    }
    buf.append("ORGANIZER;CN=\"").append(Util.tidyForIcal(organizerName))
    buf.append("\"\n")

    buf.append("UID:").append(Util.despace(start))
    buf.append("-").append(Util.despace(name))
    buf.append("\n")

    buf.append("END:VEVENT\n")
    buf.toString
  }

  def main( args:Array[String] ): Unit = {
    val iCalOut = args(0)

    val baseUrl = "http://events.westchestergov.com"

    val categoryStartUrl = "http://events.westchestergov.com/eventsbycategory/-"
    val categoryStartDoc = Util.urlToDoc(categoryStartUrl)

    val categoryTag = categoryStartDoc.select("select[id=category_fv]")
    val catPagePrefix = "http://events.westchestergov.com/eventsbycategory/"

    val allCategories = categoryTag.select("option").map(
    	(e: org.jsoup.nodes.Element) =>
	    (e.attr("Value"), Util.despace(e.text)))

    val categories = allCategories.filter(_._2 == "CountyMeetings")

    val catToUrls = categories.map{ case (catNum, catName)=> {
      val pg = Util.urlToDoc(catPagePrefix + catNum)
        (catName, pg.select("a.ev_link_row").map(
	  baseUrl + _.attr("href")).map(stripCategory _).toSet)
	}}.toMap

    val evUrlToCat = 
      reverse(catToUrls).map(x => (x._1, x._2.filter(_ != "Allcategories")))

    val urls = evUrlToCat.keys

    val icals = urls.view.map(
    	url => eventPageToiCal(url, Util.urlToDoc(url), evUrlToCat(url)))

    val out_writer =
      new BufferedWriter(new OutputStreamWriter(new FileOutputStream(iCalOut)))

    out_writer.write(Util.iCalHeader("Westchester County Calendar"))

    icals foreach out_writer.write

    out_writer.write(Util.iCalFooter)

    out_writer.close()
  }

}