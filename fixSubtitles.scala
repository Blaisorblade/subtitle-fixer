import io.Source

object Secs {
  //XXX just for fun
  class SecOps(x: Int) {
    def sec = x * 1000
  }
  implicit def toSecOps(x: Int): SecOps = new SecOps(x)
}

object FixSubtitles extends App {
  import Secs._

  //Config - add cmdline opt parser!
  val deltaMs = 0 //(-10 sec) - 700
  //This is useful to convert subs for e.g. 25FPS to subs for e.g. 23.976FPS.
  val origFps = 25.0
  val targetFps = 23.976
  val timeMultiplier = 1 //origFps / targetFps //? Or the inverse?

  //val fName = "David+Lynch+-+Twin+Peaks+-+Fire+Walk+With+Me+%281992%29 - orig.srt"
  def input =
    //Source.fromFile(fName)
    Source.stdin

  def parse(time: String): Int = {
    val Time = "(..):(..):(..),(...)".r
    time match {
      case Time(h, m, s, ms) => ((((h.toInt * 60) + m.toInt) * 60) + s.toInt) * 1000 + ms.toInt
    }
  }

  def printDate(tm: (Int, Int, Int, Int)) = {
    val (h, m, s, ms) = tm
    "%02d:%02d:%02d,%03d" format (h, m, s, ms)
  }

  def split(cumTimeMs: Int) = {
    def divMod(quot: Int, div: Int) = (quot / div, quot % div)
    val cumMs = cumTimeMs
    val (cumS, ms) = divMod(cumMs, 1000)
    val (cumM, s) = divMod(cumS, 60)
    val (h, m) = divMod(cumM, 60)
    (h, m, s, ms)
  }

  def adjust(timeMs: Int): Int =
    (timeMs * timeMultiplier).toInt + deltaMs

  def fixedLine(line: String) = {
    val TimeLine = "(.*)-->(.*)".r

    def fixedTimestampLine(inpDate: String) = printDate(split(adjust(parse(inpDate.trim))))
    line match {
      case TimeLine(start, end) =>
        "%s --> %s" format (fixedTimestampLine(start), fixedTimestampLine(end))
      case _ => line
    }
  }

  for (line <- input.getLines()) {
    println(fixedLine(line))
  }
}
