package apprenticeshipScorecard.controllers

import org.parboiled2._

import scala.annotation.tailrec
import scala.io.StdIn
import scala.util.{Failure, Success}

object QueryParser extends App {
  repl()

  @tailrec
  def repl(): Unit = {
    // TODO: Replace next three lines with `scala.Predef.readLine(text: String, args: Any*)`
    // once BUG https://issues.scala-lang.org/browse/SI-8167 is fixed
    print("---\nEnter calculator expression > ")
    Console.out.flush()
    StdIn.readLine() match {
      case "" =>
      case line =>
        val parser = new QueryParser(line)
        parser.InputLine.run() match {
          case Success(exprAst) => println("Result: " + exprAst)
          case Failure(e: ParseError) => println("Expression is not valid: " + parser.formatError(e))
          case Failure(e) => println("Unexpected error during parsing run: " + e)
        }
        repl()
    }
  }


  // our abstract syntax tree model
  sealed trait Expr

  case class Value(value: String) extends Expr

  case class GT(lhs: Expr, rhs: Expr) extends Expr

  case class LT(lhs: Expr, rhs: Expr) extends Expr

  case class EQ(lhs: Expr, rhs: Expr) extends Expr

  case class AND(lhs: Expr, rhs: Expr) extends Expr

  case class OR(lhs: Expr, rhs: Expr) extends Expr

}

/**
  * This parser reads simple calculator expressions and builds an AST
  * for them, to be evaluated in a separate phase, after parsing is completed.
  */
class QueryParser(val input: ParserInput) extends Parser {

  import QueryParser._

  def InputLine = rule {
    Expression ~ EOI
  }

  def Expression: Rule1[Expr] = rule {
    Term ~ zeroOrMore(
      '>' ~ Term ~> GT
        | '<' ~ Term ~> LT)
  }

  def Term = rule {
    Factor ~ zeroOrMore(
      '=' ~ Factor ~> EQ
        | "AND" ~ Factor ~> AND
        | "OR" ~ Factor ~> AND)
  }

  def Factor = rule {
    X | Parens
  }

  def Parens = rule {
    '(' ~ Expression ~ ')'
  }

  def X = rule {
    capture(AlphaNum) ~> Value
  }

  def AlphaNum = rule {
    oneOrMore(CharPredicate.AlphaNum)
  }
}