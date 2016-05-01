package com.wellfactored.restless.play.json

import play.api.libs.json._

/**
  * This is an execution engine to apply a Query AST to a document in the form of a JsObject.
  *
  * TODO: combine SEQ/SNEQ with EQ/NEQ
  */
object JsonQuerying {

  import com.wellfactored.restless.QueryAST._

  def query(q: Query)(implicit doc: JsObject): Boolean = {
    q match {
      case SEQ(path, s) => testString(path)(_ == s)
      case SNEQ(path, s) => testString(path)(_ != s)
      case StartsWith(path, s) => testString(path)(_.startsWith(s))
      case EndsWith(path, s) => testString(path)(_.endsWith(s))
      case Contains(path, s) => testString(path)(_.contains(s))

      case EQ(path, ref) => testNumber(path, ref)(_ == _)
      case NEQ(path, ref) => testNumber(path, ref)(_ != _)
      case GT(path, ref) => testNumber(path, ref)(_ > _)
      case GE(path, ref) => testNumber(path, ref)(_ >= _)
      case LT(path, ref) => testNumber(path, ref)(_ < _)
      case LE(path, ref) => testNumber(path, ref)(_ < _)

      case AND(q1, q2) => query(q1) && query(q2)
      case OR(q1, q2) => query(q1) || query(q2)
    }
  }

  def testString(path: Path)(test: (String) => Boolean)(implicit doc: JsObject): Boolean = {
    lookup(path) match {
      case JsString(j) => test(j)
      case _ => false
    }
  }

  def testNumber(path: Path, ref: NumberRef)(test: (Double, Double) => Boolean)(implicit doc: JsObject): Boolean = {
    ref match {
      case NumberConstant(d) => testNumber(path)(test(_, d))
      case NumberPath(p) => lookup(p) match {
        case JsNumber(j) => testNumber(path)(test(_, j.doubleValue()))
        case _ => false
      }
    }
  }

  def testNumber(path: Path)(test: (Double) => Boolean)(implicit doc: JsObject): Boolean = {
    lookup(path) match {
      case JsNumber(j) => test(j.doubleValue())
      case _ => false
    }
  }

  def lookup(path: Path)(implicit doc: JsObject): JsValue = {
    path.names.foldLeft(doc: JsValue) { case (r, s) => (r \ s).getOrElse(JsNull) }
  }
}
