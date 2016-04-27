package apprenticeshipScorecard

import apprenticeshipScorecard.queries.{QueryAST, QueryParser}
import apprenticeshipScorecard.queries.QueryAST._
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

}
