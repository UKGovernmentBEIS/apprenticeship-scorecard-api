package uk.gov.bis.apprenticeshipScorecard

import atto.Atto._
import atto.ParseResult.Done
import com.wellfactored.restless.QueryAST._
import com.wellfactored.restless.{QueryAST, QueryParser}
import play.api.libs.json.{JsError, _}
import play.api.mvc.{PathBindable, QueryStringBindable}

import scala.util.Try

object bindings {

  implicit val bdBinding = new PathBindable[BigDecimal] {
    override def bind(key: String, value: String): Either[String, BigDecimal] = {
      Try(BigDecimal(value)).toOption match {
        case Some(bd) => Right(bd)
        case None => Left(s"could not convert parameter $key to a number")

      }
    }

    override def unbind(key: String, value: BigDecimal): String = value.toString()
  }

  implicit val queryBinding = new QueryStringBindable[QueryAST.Query] {
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
