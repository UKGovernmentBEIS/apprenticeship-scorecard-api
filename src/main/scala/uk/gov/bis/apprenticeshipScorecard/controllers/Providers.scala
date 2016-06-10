package uk.gov.bis.apprenticeshipScorecard.controllers

import javax.inject.Inject

import com.wellfactored.restless.play.actions.ApiActions._
import play.api.libs.json.{JsObject, Json}
import play.api.mvc.{Action, Controller}
import uk.gov.bis.apprenticeshipScorecard.models.{Apprenticeship, Provider, UKPRN}
import uk.gov.bis.apprenticeshipScorecard.tools.{ProviderIndex, TSVLoader}

import scala.concurrent.ExecutionContext

class Providers @Inject()(implicit ec: ExecutionContext) extends Controller {

  implicit val providerFormat = Json.format[Provider]
  implicit val apprenticeshipFormat = Json.format[Apprenticeship]

  import TSVLoader.dataStore

  def find(ukprn: UKPRN) = Action { implicit request =>
    dataStore.providers.get(ukprn) match {
      case None => NotFound
      case Some(p) => Ok(Json.toJson(p))
    }
  }

  def search(phrase: String) = JsCollect(ProviderIndex.matchPhrase(phrase).sortBy(_.rank).map(Json.toJson(_).as[JsObject]))

  def apprenticeships(ukprn: UKPRN) = JsCollect(dataStore.apprenticeships.filter(_.provider_id == ukprn).sortBy(_.description).map(Json.toJson(_).as[JsObject]))

  def providers = JsCollect(dataStore.providersJs)
}
