import io.Source
import java.io.{BufferedWriter, PrintWriter, FileWriter, File,
  OutputStreamWriter}
import scopt.immutable.OptionParser

object Secs {
  //XXX just for fun
  class SecOps(x: Int) {
    def sec = x * 1000
  }
  implicit def toSecOps(x: Int): SecOps = new SecOps(x)
}

trait Logging {
  def warn(msg: Any) = Console.err.println("Warning: " + msg)
  def error(msg: Any)(implicit parser: OptionParser[_]): Nothing = {
    Console.err.println("Error: " + msg)
    Console.err.println(parser.usage)
    System.exit(1)
    throw new Throwable //Just to get the Nothing return type.
  }
}

case class Config(deltaMs: Int = 0, origFps: Option[Double] = None,
  targetFps: Option[Double] = None,
  inputFile: Option[String] = None, outputFile: Option[String] = None)

object FixSubtitles extends App with Logging {
  import Secs._

  implicit val parser = new OptionParser[Config]("subtitle-fixer", "0.1") { def options = Seq(
    intOpt("d", "deltaMs", "how much earlier than they should do subtitles" +
      " appear (that is, time interval to add to subtitles timestamp); can be" +
      " negative") { (v, c) => c.copy(deltaMs = v) },
    doubleOpt("o", "origFps", "framerate the original subtitle is for; only" +
      " needed for framerate conversion") {(v, c) => c.copy(origFps = Some(v))},
    doubleOpt("t", "targetFps", "") {(v, c) => c.copy(targetFps = Some(v))},
    argOpt("<input subtitle>", "(std. input if not specified)") {(v, c) =>
      c.copy(inputFile = Some(v))},
    argOpt("<output subtitle>", "(same as <input file> if not specified; if both" +
      " are not specified, then the std. output)") {(v, c) =>
      c.copy(outputFile = Some(v))}
  )}
  parser.parse(args, Config()) map { config =>
    import config._
//    //Config - add cmdline opt parser!
//    val deltaMs = 0 //(-10 sec) - 700
//    //This is useful to convert subs for e.g. 25FPS to subs for e.g. 23.976FPS.
//    val origFps = 25.0
//    val targetFps = 23.976
//    val timeMultiplier = 1 //origFps / targetFps //? Or the inverse?
    val timeMultiplier = (origFps, targetFps) match {
      case (Some(orig), Some(target)) =>
        orig / target //? Or the inverse?
      case otherwise =>
        otherwise match {
          case (None, None) =>
            //Omitting both is allowed, do nothing.
          case _ =>
            error("origFps and targetFps must be specified together")
        }
        if (deltaMs == 0)
          error("no change specified")
        1
    }

    val input =
      inputFile match {
        case Some(fName) =>
          Source.fromFile(fName)
        case _ =>
          Source.stdin
      }

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

    val outputSub = for (line <- input.getLines()) yield fixedLine(line)

    val output = outputFile orElse inputFile match {
      case Some(fName) =>
        val f = new File(fName)
        if (f.exists)
          new PrintWriter(new BufferedWriter(new FileWriter(fName)))
        else
          error("input file not found!")
      case _ =>
        new PrintWriter(new OutputStreamWriter(Console.out))
    }

    for (line <- outputSub) {
      output.println(line)
    }
  } getOrElse {
      // arguments are bad, usage message will have been displayed
  }
}
