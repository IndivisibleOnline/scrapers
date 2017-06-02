package org.indivisiblewestchester.county

import org.indivisiblewestchester.Util

import com.fasterxml.jackson.core.JsonParser
import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.module.scala.DefaultScalaModule

import java.io.BufferedWriter
import java.io.OutputStreamWriter
import java.io.FileOutputStream

object LegCalScraper {
  def main( args:Array[String] ):Unit = {
    val iCalOut = args(0)

    val baseUrl = "http://westchestercountyny.iqm2.com"
    val startUrl = baseUrl + "/Citizens/Calendar.aspx?View=Calendar"

    val startDoc = Util.urlToDoc(startUrl)
    val jsonStr =
      startDoc.select("input[id=ContentPlaceholder1_hfEventJSON]").attr("value")

    val objectMapper = new ObjectMapper() // with ScalaObjectMapper
    objectMapper.registerModule(DefaultScalaModule)

    objectMapper.configure(JsonParser.Feature.ALLOW_UNQUOTED_FIELD_NAMES, true);
    objectMapper.configure(JsonParser.Feature.ALLOW_SINGLE_QUOTES, true);
    val meetings = objectMapper.readValue(jsonStr, classOf[Array[LegCalMeeting]])

    val iCalWriter =
      new BufferedWriter(new OutputStreamWriter(new FileOutputStream(iCalOut)))

      iCalWriter.write("""BEGIN:VCALENDAR
VERSION:2.0
PRODID:WestchesterLegislativeCalendar
METHOD:PUBLISH
""")

    meetings.map(_.getEvent).map(
      Util.mapToIcal(_)).foreach(iCalWriter.write)

    iCalWriter.write("""END:VCALENDAR
""")

    iCalWriter.close()

  }
}

