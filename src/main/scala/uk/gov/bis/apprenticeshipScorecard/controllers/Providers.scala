package uk.gov.bis.apprenticeshipScorecard.controllers

import javax.inject.Inject

import com.wellfactored.restless.QueryAST.Query
import play.api.libs.json.Json
import play.api.mvc.{Action, Controller}
import uk.gov.bis.apprenticeshipScorecard.models.{Apprenticeship, Provider, UKPRN}
import uk.gov.bis.apprenticeshipScorecard.tools.TSVLoader

import scala.concurrent.ExecutionContext

class Providers @Inject()(implicit ec: ExecutionContext) extends Controller{
  import Selector._

  implicit val providerFormat = Json.format[Provider]
  implicit val apprenticeshipFormat = Json.format[Apprenticeship]

  import TSVLoader.dataStore

  def find(ukprn: Long) = Action { implicit request =>
    dataStore.providers.get(UKPRN(ukprn)) match {
      case None => NotFound
      case Some(p) => Ok(Json.toJson(p))
    }
  }

  def apprenticeships(ukprn: Long, page_number: Option[Int], page_size: Option[Int], max_results: Option[Int], q: Option[Query]) = Action { implicit request =>
    val params = Params(page_number, page_size, max_results, q, None)
    val results = dataStore.apprenticeships.filter(_.provider_id == UKPRN(ukprn))

    Ok(Json.toJson(results))
  }

  def get(page_number: Option[Int], page_size: Option[Int], max_results: Option[Int], q: Option[Query]) = Action {
    val params = Params(page_number, page_size, max_results, q, None)
    val results = dataStore.providers.values.select(params)(_.name)

    Ok(Json.toJson(results))
  }

  def post = Action(parse.json) { request =>
    jsonAction(request.body) { params: Params => dataStore.providers.values.select(params)(_.name) }
  }
}
