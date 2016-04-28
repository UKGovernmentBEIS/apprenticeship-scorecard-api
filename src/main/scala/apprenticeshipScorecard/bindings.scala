package apprenticeshipScorecard

import atto.ParseResult.Done
import com.wellfactored.restless.{QueryAST, QueryParser}
import com.wellfactored.restless.QueryAST._
import play.api.libs.json.{JsError, _}
import play.api.mvc.QueryStringBindable

object bindings {
  implicit val queryBinding = new QueryStringBindable[QueryAST.Query] {

    import atto._
    import Atto._
    import ParseResult._

    override def bind(key: String, params: Map[String, Seq[String]]): Option[Either[String, Query]] = {
      params.get(key).map { qs =>
        QueryParser.query.parseOnly(qs.headOption.getOrElse("")) match {
          case Done(_, q) => Right(q)
          case _ => Left("failed to parse query string")
        }
      }
    }

    override def unbind(key: String, value: Query): String = ???
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
