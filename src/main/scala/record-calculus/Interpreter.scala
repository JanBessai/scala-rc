package record_calculus
import scala.language.implicitConversions
import record_calculus.parsers.FullCalculusParsers
import record_calculus.prettyprinters.FullPrettyPrinter
import scala.io.Source

object Interpreter extends App {
  val parser = new FullCalculusParsers {
    lazy val calculus = new FullCalculus with FullPrettyPrinter {
      override def whnfEval(term: Term, rhss: Seq[Term]): Term = {
        //println(s"Evaluating: ${pretty(term2Doc(term), 80)}")
        super.whnfEval(term, rhss)
      }
    }
  }
  val calculus: parser.calculus.type = parser.calculus
  import calculus.{pretty, eval, term2Doc}

  args match {
    case Array(file) =>
      try {
        val source = Source.fromFile(file).reader()
        val program = parser.parseAll(parser.term, source) match {
          case parser.Success(result, _) => result
          case fail: parser.NoSuccess => sys.error(s"Cannot parse $file: ${fail.toString}")
        }
        println("Program:")
        println(s"${pretty(program)}")
        println()
        println("Result:")
        println(s"${pretty(eval(program))}")
      } catch {
        case ex : Throwable =>
          ex.printStackTrace()
          println(s"Exception: ${ex.toString}")
      }
    case _ =>
      println(s"Usage: Interpreter SOURCE_FILE")
  }
}

