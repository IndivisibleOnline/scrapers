package org.indivisiblewestchester.county

import java.io.StringWriter
import java.net.URLDecoder
import java.time.format.DateTimeFormatter
import java.time.LocalDateTime

import org.indivisiblewestchester.Util

object LegCalMeeting {
  private var baseUrl = "http://westchestercountyny.iqm2.com"
  def setBaseUrl(base: String) = { baseUrl = base }
}

case class LegCalMeeting(var title: String = "",
     	     	   	 var start: String = "",
		   	 var allday: String = "",
		   	 var url: String = "",
		   	 var video: String = "",
		   	 var downloads: String = "",
		   	 var cancelled : String = "" ) {
  def startDate = {
    val dateFormat =  "uuuu-MM-dd k:m"
    val parseFormatter = DateTimeFormatter.ofPattern(dateFormat)
    val outFormatter = DateTimeFormatter.ofPattern("yyyyMMdd'T'HHmmss")
    outFormatter.format(LocalDateTime.parse(start, parseFormatter))
  }
  def UIDDate = {
	val dateFormat =  "uuuu-MM-dd k:m"
	val dateFormatter = DateTimeFormatter.ofPattern("MM-dd-yyyy")
	val parseFormatter = DateTimeFormatter.ofPattern(dateFormat)
	dateFormatter.format(parseFormatter.parse(start))
  }
  def fixupUrl(host: String, prevUrl: String, newUrl: String) = {
    if (newUrl.startsWith("/"))
      host + newUrl
    else {
      if (prevUrl.startsWith("http:"))
        prevUrl
      else 
        host + prevUrl
      }.reverse.dropWhile(_ != '/').reverse + newUrl
  }
  def fullUrl = LegCalMeeting.baseUrl + url;
  lazy val videoUrl = if (video != null) {
      Option(LegCalMeeting.baseUrl +
        URLDecoder.decode(video.replaceAll("!dq!","\"").replaceAll("!sc!",";")
	  ).replaceAll("&amp;", "&").split("&quot;")(1))
    } else None
  lazy val detailPage =  Util.urlToDoc(fullUrl)
  lazy val getVenue = {
    val addrStr = detailPage.select("div[class=MeetingAddress]").text
    val addrbits = addrStr.split("\\xa0").filter(_.length != 0)
    val (addr1, addr2) = ( if (addrbits.length == 1)
          (addrStr, "")
	else
	  (addrbits(0), addrbits.drop(1).mkString(", ")))

    if (addrbits.mkString(", ") == "148 Martine Avenue, 8th Floor, White Plains, NY 10601") {
      Map("LOCATION" -> ("Westchester County Board of Legislators - Michaelian Office Building\n"
                         + "148 Martine Avenue, 8th Floor\n"
			 + "White Plains, New York 10601 USA"))
	//"(914) 995-2800", "http://westchesterlegislators.com/contact-us.html")
    } else if (addrbits.mkString(", ") == "198 Central Avenue, White Plains, NY 10606") {
      Map("LOCATION" -> ("Westchester County Center\n"
      	  		 + "198 Central Avenue\n"
			 + "White Plains, NY 10606 USA\n"))
	//"(914) 995-4050", "http://www.countycenter.biz/")
    } else
      Map("LOCATION" -> addrStr)
  }
  lazy val getOrganizer = {
    val group = detailPage.select("span[id=ContentPlaceholder1_lblMeetingGroup]").text
    Map("ORGANIZER" -> group)
  }
  lazy val getEvent = {
    val name = detailPage.select("span[id=ContentPlaceholder1_lblMeetingType]").text

    val cats = List("CountyLegCalendar", 
    	getOrganizer.getOrElse("Location", ""), name).map(
    	  _.filter(_.isWhitespace == false)).filter(_.size != 0).mkString(", ")

    val desc = new StringWriter

    if (downloads.size > 0) {
      val parsedDl = org.jsoup.Jsoup.parse(
	URLDecoder.decode(downloads.replaceAll("!dq!","\"").replaceAll("!sc!","")))

      List(parsedDl.select("a[href]")).map(e => {
        val tgt =  e.attr("href")
        if (!tgt.startsWith("/") && !tgt.startsWith("http:"))
          e.attr("href", fixupUrl(LegCalMeeting.baseUrl, url, tgt))
      } )
      desc.append(parsedDl.select("ul").toString)
    }
 
    if (videoUrl != None) {
       desc.append(""" <a href='""").append(videoUrl.get)
       desc.append("""' target='_blank'>Video</a><br/><br/>""")
    }

    val deets = detailPage.select("table[id=MeetingDetail]")
    if (deets.size > 0)
      desc.append(detailPage.select("table[id=MeetingDetail]").text.take(1000))

    Map("SUMMARY"-> ("Westchester: " + title),
        "DTSTART"-> startDate,
        "CATEGORIES"->  cats, 
	"URL"-> fullUrl,
	"DESCRIPTION"-> desc.toString,
	"UID" -> (Util.despace(start) + "-" + Util.despace(name))
	) ++ getVenue ++ getOrganizer
	
  }
}




