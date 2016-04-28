package com.wellfactored.restless

import atto.Atto._
import atto.ParseResult._
import atto._

import scala.annotation.tailrec
import scala.io.StdIn

object QueryParserApp extends App {
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
        QueryParser.query.parseOnly(line) match {
          case Fail(_, _, err) => println(err)
          case Partial(_) =>
          case Done(_, p) => println("Result: " + p)
        }
        repl()
    }
  }
}

// our abstract syntax tree model
object QueryAST {

  sealed trait Query

  case class Path(names: List[String])

  sealed trait NumberRef

  case class NumberConstant(d: Double) extends NumberRef

  case class NumberPath(p: Path) extends NumberRef

  sealed trait Comparison extends Query

  sealed trait NumberComparison extends Comparison

  case class GT(lhs: Path, rhs: NumberRef) extends NumberComparison

  case class GE(lhs: Path, rhs: NumberRef) extends NumberComparison

  case class LT(lhs: Path, rhs: NumberRef) extends NumberComparison

  case class LE(lhs: Path, rhs: NumberRef) extends NumberComparison

  case class EQ(lhs: Path, rhs: NumberRef) extends NumberComparison

  case class NEQ(lhs: Path, rhs: NumberRef) extends NumberComparison

  sealed trait StringComparison extends Comparison

  case class SEQ(lhs: Path, rhs: String) extends StringComparison

  case class SNEQ(lhs: Path, rhs: String) extends StringComparison

  case class StartsWith(lhs: Path, rhs: String) extends StringComparison

  case class EndsWith(lhs: Path, rhs: String) extends StringComparison

  case class Contains(lhs: Path, rhs: String) extends StringComparison

  sealed trait Conjunction extends Query

  case class AND(lhs: Query, rhs: Query) extends Conjunction

  case class OR(lhs: Query, rhs: Query) extends Conjunction

  trait Conj {
    def make(left: Query, right: Query): Conjunction
  }

  object Conj {

    case object and extends Conj {
      override def make(left: Query, right: Query): Conjunction = AND(left, right)
    }

    case object or extends Conj {
      override def make(left: Query, right: Query): Conjunction = OR(left, right)
    }

  }

}

object QueryParser extends Whitespace {

  import QueryAST._

  lazy val query: Parser[Query] = {
    conjunction.t | comparison.t <~ endOfInput
  }.named("query")

  lazy val conjunction: Parser[Conjunction] = {
    (comparison.t ~ conj.t ~ conjunction).map { case ((l, c), r) => c.make(l, r) } |
      (comparison.t ~ conj.t ~ comparison).map { case ((l, c), r) => c.make(l, r) } |
      (parens(conjunction).t ~ conj.t ~ comparison).map { case ((l, c), r) => c.make(l, r) } |
      (comparison.t ~ conj.t ~ parens(conjunction)).map { case ((l, c), r) => c.make(l, r) } |
      (parens(conjunction).t ~ conj.t ~ parens(conjunction)).map { case ((l, c), r) => c.make(l, r) }
  }.named("conjunction")

  lazy val conj: Parser[Conj] = and | or

  lazy val and = string("and") >| Conj.and
  lazy val or = string("or") >| Conj.or

  lazy val identifier: Parser[String] = {
    val startingChar: Parser[Char] = elem(c => c.isLetter || c == '_')
    val identifierChar: Parser[Char] = elem(c => c.isLetterOrDigit || c == '_')

    (startingChar ~ many(identifierChar)).map { case (c, cs) => c + cs.mkString }
  }.named("identifier")

  lazy val path: Parser[Path] = {
    (identifier ~ many(char('.') ~> identifier)).map { case (s, rest) => Path(List(s) ++ rest) }
  }.named("path")

  lazy val comparison: Parser[Comparison] = {
    numberComparison | stringComparison
  }

  lazy val stringComparison: Parser[StringComparison] = {
    pairByT(path, char('='), stringLiteral).map(SEQ.tupled) |
      pairByT(path, string("!="), stringLiteral).map(SNEQ.tupled) |
      pairByT(path, string("starts-with"), stringLiteral).map(StartsWith.tupled) |
      pairByT(path, string("ends-with"), stringLiteral).map(EndsWith.tupled) |
      pairByT(path, string("contains"), stringLiteral).map(Contains.tupled)
  }.named("string comparison")

  lazy val numberComparison: Parser[NumberComparison] = {
    pairByT(path, char('='), numberRef) -| EQ.tupled |
      pairByT(path, string("!="), numberRef) -| NEQ.tupled |
      pairByT(path, char('<'), numberRef).map(LT.tupled) |
      pairByT(path, char('>'), numberRef).map(GT.tupled) |
      pairByT(path, string("<="), numberRef).map(LE.tupled) |
      pairByT(path, string(">="), numberRef).map(GE.tupled)
  }.named("number comparison")

  lazy val numberRef: Parser[NumberRef] = {
    double -| NumberConstant |
      path -| NumberPath
  }


}

// Some extra combinators and syntax for coping with whitespace. Something like this might be
// useful in core but it needs some thought - borrowed from the atto source
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