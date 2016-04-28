package apprenticeshipScorecard

import apprenticeshipScorecard.queries.{JsonQuerying, QueryParser}
import apprenticeshipScorecard.queries.QueryAST.Query
import atto.ParseResult.Done
import play.api.libs.json.{JsError, JsPath, JsSuccess, _}


package object controllers {

  implicit class Filtering[T: Writes](xs: Seq[T]) {
    def limit(l: Option[Int]): Seq[T] = l match {
      case Some(i) => xs.take(i)
      case None => xs
    }

    def query(qo: Option[Query]): Seq[T] = qo match {
      case None => xs
      case Some(q) => xs.filter { x =>
        Json.toJson(x) match {
          case doc: JsObject => JsonQuerying.query(q)(doc)
          case _ => false
        }
      }
    }
  }

  implicit val queryR = new Reads[Query] {

    import atto._
    import Atto._

    override def reads(json: JsValue): JsResult[Query] = {
      implicitly[Reads[String]].reads(json).flatMap { qs =>
        QueryParser.query.parseOnly(qs) match {
          case Done(_, q) => JsSuccess(q, JsPath())
          case _ => JsError("failed to parse query string")
        }
      }
    }
  }
}

