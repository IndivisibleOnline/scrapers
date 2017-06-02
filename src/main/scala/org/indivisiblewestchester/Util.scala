package org.indivisiblewestchester

import java.io.StringWriter

object Util {
  private var pageCache:
    Option[collection.mutable.Map[String, org.jsoup.nodes.Document]]= None

  def enablePageCache = {
    pageCache = Some(collection.mutable.Map[String, org.jsoup.nodes.Document]())
  }

  def urlToDoc(url: String): org.jsoup.nodes.Document = {
    lazy val doc = org.jsoup.Jsoup.connect(url).get()
    if (pageCache.isDefined)   
      pageCache.get.getOrElseUpdate(url, doc)
    else
      doc
  }

  val despacePat = """(\W|-)+""".r

  def despace(in: String) = despacePat replaceAllIn(in, "")

  def tidyForIcal(in: String) =
    in.replaceAll("\u00a0"," ").replaceAll("\n","\\\\n").trim.stripSuffix(",").trim

  def mapToIcal(in: Map[String,Any]) = {
    val buf = new StringWriter()

    buf.append("BEGIN:VEVENT\n")
    in.map(p => buf.append(p._1 +":" + tidyForIcal(p._2.toString) + "\n"))
    buf.append("END:VEVENT\n")
    buf.toString
  }

  def iCalHeader(prodName: String) = ("""BEGIN:VCALENDAR
VERSION:2.0
PRODID: """+ prodName+ """
METHOD:PUBLISH
""")

  def iCalFooter = """END:VCALENDAR
"""

}