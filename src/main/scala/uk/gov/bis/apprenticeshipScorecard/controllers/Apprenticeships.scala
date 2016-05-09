package uk.gov.bis.apprenticeshipScorecard.controllers

import javax.inject.Inject

import atto.ParseResult.Done
import com.wellfactored.restless.QueryAST.Path
import com.wellfactored.restless.QueryParser
import play.api.libs.json._
import play.api.mvc._
import uk.gov.bis.apprenticeshipScorecard.models._
import uk.gov.bis.apprenticeshipScorecard.tools.TSVLoader

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try
import scala.util.control.NonFatal

class Apprenticeships @Inject()(implicit ec: ExecutionContext) extends Controller {

  import Selector._

  implicit val providerFormat = Json.format[Provider]
  implicit val apprenticeshipFormat = Json.format[Apprenticeship]

  import TSVLoader.dataStore

  def apprenticehships = Action(parse.tolerantText) { implicit request =>
    extractParams.fold(
      result => result,
      params => Ok(Json.toJson(dataStore.apprenticeships.select(params)(_.description)))
    )
  }

  def post = Action(parse.json) { request =>
    jsonAction(request.body) { params: Params => dataStore.apprenticeships.select(params)(_.description) }
  }

  def extractParams(implicit request: Request[String]): Either[Result, Params] = {
    request.method match {
      case "POST" => extractFromJson
      case "GET" => extractFromQueryParams(request.queryString)
      case m => Left(BadRequest(s"Invalid html method type: $m"))
    }
  }

  /**
    * TODO: handle errors in the query parser
    */
  def extractFromQueryParams(params: Map[String, Seq[String]]): Either[Result, Params] = {

    import atto._
    import Atto._

    val pageNumber = params.get("page_number").flatMap(_.headOption.map(_.toInt))
    val pageSize = params.get("page_size").flatMap(_.headOption.map(_.toInt))
    val maxResults = params.get("max_results").flatMap(_.headOption.map(_.toInt))
    val query = params.get("query").flatMap {
      _.headOption.map { qs =>
        QueryParser.query.parseOnly(qs) match {
          case Done(_, q) => Right(q)
          case _ => Left("failed to parse query string")
        }
      }
    }
    val fields = params.get("fields").flatMap {
      _.headOption.flatMap { s =>
        Try(Json.parse(s)).toOption.flatMap { jv =>
          jv match {
            case JsArray(vs) => Some(vs.toList.flatMap(_.validate[String].asOpt.map(f => Path(f.split('.').toList))))
            case _ => None
          }
        }
      }
    }

    Right(Params(pageNumber, pageSize, maxResults, query.map(_.right.get), fields))
  }

  def extractFromJson(implicit request: Request[String]): Either[Result, Params] = {
    try {
      Json.parse(request.body).validate[Params].fold(
        errs => Left(BadRequest(errs.toString())),
        params => Right(params)
      )
    } catch {
      case NonFatal(e) => Left(BadRequest(e.getMessage))
    }
  }
}
