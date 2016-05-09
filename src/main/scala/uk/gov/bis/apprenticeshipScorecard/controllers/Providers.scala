package uk.gov.bis.apprenticeshipScorecard.controllers

import javax.inject.Inject

import play.api.libs.json.Json
import play.api.mvc.{Action, Controller}
import uk.gov.bis.apprenticeshipScorecard.ApiActions.Collection
import uk.gov.bis.apprenticeshipScorecard.models.{Apprenticeship, Provider, UKPRN}
import uk.gov.bis.apprenticeshipScorecard.tools.TSVLoader

import scala.concurrent.ExecutionContext

class Providers @Inject()(implicit ec: ExecutionContext) extends Controller {

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

  def apprenticeships(ukprn: Long) = Collection { implicit request =>
    val results = dataStore.apprenticeships.filter(_.provider_id == UKPRN(ukprn)).select(request.params)(_.description)
    Ok(Json.toJson(results))
  }

  def providers = Collection { implicit request =>
    Ok(Json.toJson(dataStore.providers.values.select(request.params)(_.name)))
  }
}
