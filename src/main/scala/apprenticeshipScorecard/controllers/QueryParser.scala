package apprenticeshipScorecard.controllers

import atto._
import atto.Atto._
import atto.ParseResult._

import scala.annotation.tailrec
import scala.io.StdIn

object QueryParser extends App {
  repl()

  @tailrec
  def repl(): Unit = {
    // TODO: Replace next three lines with `scala.Predef.readLine(text: String, args: Any*)`
    // once BUG https://issues.scala-lang.org/browse/SI-8167 is fixed
    print("---\nEnter expression > ")
    Console.out.flush()
    StdIn.readLine() match {
      case "" =>
      case line =>
        ExpressionParser.expr.parseOnly(line) match {
          case Fail(_, _, err) => println(err)
          case Partial(_) =>
          case Done(_, p) => println("Result: " + p)
        }
        repl()
    }
  }


  // our abstract syntax tree model
  sealed trait Expr

  case class Path(names: List[String])

  trait Comparison extends Expr

  trait NumberComparison extends Comparison

  case class GT(lhs: Path, rhs: Double) extends NumberComparison

  case class GE(lhs: Path, rhs: Double) extends NumberComparison

  case class LT(lhs: Path, rhs: Double) extends NumberComparison

  case class LE(lhs: Path, rhs: Double) extends NumberComparison

  case class EQ(lhs: Path, rhs: Double) extends NumberComparison

  case class NEQ(lhs: Path, rhs: Double) extends NumberComparison

  trait StringComparison extends Comparison

  case class SEQ(lhs: Path, rhs: String) extends StringComparison

  case class SNEQ(lhs: Path, rhs: String) extends StringComparison

  case class StartsWith(lhs: Path, rhs: String) extends StringComparison

  case class EndsWith(lhs: Path, rhs: String) extends StringComparison

  case class Contains(lhs: Path, rhs: String) extends StringComparison

  trait Conjunction extends Expr

  case class AND(lhs: Expr, rhs: Expr) extends Conjunction

  case class OR(lhs: Expr, rhs: Expr) extends Conjunction

  trait Conj {
    def make(left: Expr, right: Expr): Conjunction
  }

  object Conj {

    case object and extends Conj {
      override def make(left: Expr, right: Expr): Conjunction = AND(left, right)
    }

    case object or extends Conj {
      override def make(left: Expr, right: Expr): Conjunction = OR(left, right)
    }

  }

}

object ExpressionParser extends Whitespace {

  import QueryParser._

  lazy val expr: Parser[Expr] = delay {
    parens(comparison) |
      comparison |
      parens(conjunction) |
      conjunction
  }

  lazy val identifier: Parser[String] = delay {
    val startingChar: Parser[Char] = elem(c => c.isLetter || c == '_')
    val identifierChar: Parser[Char] = elem(c => c.isLetterOrDigit || c == '_')

    (startingChar ~ many(identifierChar)).map { case (c, cs) => c + cs.mkString }
  }.named("identifier")

  lazy val path: Parser[Path] = delay {
    (identifier ~ many(char('.') ~> identifier)).map { case (s, rest) => Path(List(s) ++ rest) }
  }.named("path")

  lazy val comparison: Parser[Comparison] = delay {
    numberComparison | stringComparison
  }

  lazy val stringComparison: Parser[StringComparison] = delay {
    pairByT(path, char('='), stringLiteral).map(SEQ.tupled) |
      pairByT(path, string("!="), stringLiteral).map(SNEQ.tupled) |
      pairByT(path, string("starts-with"), stringLiteral).map(StartsWith.tupled) |
      pairByT(path, string("ends-with"), stringLiteral).map(EndsWith.tupled) |
      pairByT(path, string("contains"), stringLiteral).map(Contains.tupled)
  }

  lazy val numberComparison: Parser[NumberComparison] = delay {
    pairByT(path, char('='), double) -| EQ.tupled |
      pairByT(path, string("!="), double) -| NEQ.tupled |
      pairByT(path, char('<'), double).map(LT.tupled) |
      pairByT(path, char('>'), double).map(GT.tupled) |
      pairByT(path, string("<="), double).map(LE.tupled) |
      pairByT(path, string(">="), double).map(GE.tupled)
  }

  lazy val conjunction: Parser[Conjunction] = delay {
    (expr.t ~ conj.t ~ expr.t).map { case ((l, c), r) => c.make(l, r) }
  }

  lazy val conj: Parser[Conj] = delay {
    and | or
  }

  lazy val and = string("and") >| Conj.and
  lazy val or = string("or") >| Conj.or
}

// Some extra combinators and syntax for coping with whitespace. Something like this might be
// useful in core but it needs some thought.
trait Whitespace {

  // Syntax for turning a parser into one that consumes trailing whitespace
  implicit class TokenOps[A](self: Parser[A]) {
    def t: Parser[A] =
      self <~ takeWhile(c => c.isSpaceChar || c == '\n')
  }

  // Delimited list
  def sepByT[A](a: Parser[A], b: Parser[_]): Parser[List[A]] =
    sepBy(a.t, b.t)

  // Delimited pair, internal whitespace allowed
  def pairByT[A, B](a: Parser[A], delim: Parser[_], b: Parser[B]): Parser[(A, B)] =
    pairBy(a.t, delim.t, b)

}