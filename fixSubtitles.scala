import io.{Source, Codec}
import java.io.{BufferedWriter, PrintWriter, FileWriter, File,
  OutputStreamWriter, FileOutputStream}
import scopt.OptionParser

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
  implicit val parser = new OptionParser[Config]("subtitle-fixer") {
    head("subtitle-fixer", "0.1")
    opt[Int]('d', "deltaMs") action { (v, c) => c.copy(deltaMs = v) } text
    ("how later than they do should subtitles" +
         " appear? That is, time interval to add to subtitles timestamp. It can be" +
         " negative")
    opt[Double]('o', "origFps") action {(v, c) => c.copy(origFps = Some(v))} text
      ("framerate the original subtitle is for; only" +
         " needed for framerate conversion")
    opt[Double]('t', "targetFps") action {(v, c) => c.copy(targetFps = Some(v))} text("")
    arg[String]("<input subtitle>") action {(v, c) => c.copy(inputFile = Some(v))} text
      ("(std. input if not specified)")
    arg[String]("<output subtitle>") action {(v, c) => c.copy(outputFile = Some(v))} text
      ("(same as <input file> if not specified; if both" +
         " are not specified, then the std. output)")
    }
  parser.parse(args, Config()) map { config =>
    import config._
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
          Source.fromFile(fName)(Codec.ISO8859)
        case _ =>
          Source.stdin
      }

    val output = new PrintWriter(outputFile orElse inputFile match {
      case Some(fName) =>
        val f = new File(fName)
        try {
          new BufferedWriter(new OutputStreamWriter(new
            FileOutputStream(f), "ISO-8859-1"))
        } catch {
          case ioe: java.io.IOException =>
            error("Could not open output file! Error: " + ioe)
            //error("input file not found! Name: " + fName + ", file: " + f)
        }
      case _ =>
        new OutputStreamWriter(Console.out)
    })

    def parse(time: String): Int = {
      val Time = "(..):(..):(..),(...)".r
      time match {
        case Time(h, m, s, ms) => ((((h.toInt * 60) + m.toInt) * 60) + s.toInt) * 1000 + ms.toInt
        case _ => throw new RuntimeException("Couldn't parse: \"%s\"" format time)
      }
    }

    def printDate(tm: (Int, Int, Int, Int)) = {
      val (h, m, s, ms) = tm
      "%02d:%02d:%02d,%03d" format (h, m, s, ms)
    }

    def split(cumMs: Int) = {
      def divMod(quot: Int, div: Int) = (quot / div, quot % div)
      val (cumS, ms) = divMod(cumMs, 1000)
      val (cumM, s) = divMod(cumS, 60)
      val (h, m) = divMod(cumM, 60)
      (h, m, s, ms)
    }

    def adjust(timeMs: Int): Int =
      (timeMs * timeMultiplier).toInt + deltaMs

    def fixedTimestampLine(inpDate: String) = printDate(split(adjust(parse(inpDate.trim))))

    val TimeLine = "(.*)-->(.*)".r

    for (line <- input.getLines()) {
      output.println(line match {
        case TimeLine(start, end) =>
          s"${fixedTimestampLine(start)} --> ${fixedTimestampLine(end)}"
        case _ => line
      })
    }
    output.close()
  } getOrElse {
      // arguments are bad, usage message will have been displayed
  }
}
