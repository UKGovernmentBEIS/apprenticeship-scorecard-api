package queries

import play.api.libs.json._

object JsonQuerying {

  import QueryAST._

  def query(json: JsValue, q: Query): Boolean = {
    q match {
      case SEQ(path, s) => testString(json, path)(_ == s)
      case SNEQ(path, s) => testString(json, path)(_ != s)
      case StartsWith(path, s) => testString(json, path)(_.startsWith(s))
      case EndsWith(path, s) => testString(json, path)(_.endsWith(s))
      case Contains(path, s) => testString(json, path)(_.contains(s))

      case EQ(path, d) => testNumber(json, path)(_ == d)
      case NEQ(path, d) => testNumber(json, path)(_ != d)
      case GT(path, d) => testNumber(json, path)(_ > d)
      case GE(path, d) => testNumber(json, path)(_ >= d)
      case LT(path, d) => testNumber(json, path)(_ < d)
      case LE(path, d) => testNumber(json, path)(_ < d)

      case AND(q1, q2) => query(json, q1) && query(json, q2)
      case OR(q1, q2) => query(json, q1) || query(json, q2)
    }
  }

  def testString(json: JsValue, path: Path)(test: (String) => Boolean): Boolean = {
    lookup(json, path) match {
      case JsString(j) => test(j)
      case _ => false
    }
  }

  def testNumber(json: JsValue, path: Path)(test: (Double) => Boolean): Boolean = {
    lookup(json, path) match {
      case JsNumber(j) => test(j.doubleValue())
      case _ => false
    }
  }

  def lookup(json: JsValue, path: Path): JsValue = {
    path.names.foldLeft(json) { case (r, s) => (r \ s).getOrElse(JsNull) }
  }
}
