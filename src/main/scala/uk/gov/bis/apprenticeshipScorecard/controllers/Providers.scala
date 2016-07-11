package uk.gov.bis.apprenticeshipScorecard.controllers

import javax.inject.Inject

import com.wellfactored.restless.play.actions.ApiActions._
import play.api.libs.json.{JsObject, Json}
import play.api.mvc.{Action, Controller}
import uk.gov.bis.apprenticeshipScorecard.models.{Apprenticeship, Provider, UKPRN}
import uk.gov.bis.apprenticeshipScorecard.tools.{ProviderIndex, Ranked, TSVLoader}

import scala.concurrent.ExecutionContext

class Providers @Inject()(implicit ec: ExecutionContext) extends Controller with LocationSearchSupport {

  implicit val providerFormat = Json.format[Provider]
  implicit val apprenticeshipFormat = Json.format[Apprenticeship]

  import TSVLoader.dataStore

  def find(ukprn: UKPRN) = Action { implicit request =>
    dataStore.providers.get(ukprn) match {
      case None => NotFound
      case Some(p) => Ok(Json.toJson(p))
    }
  }

  implicit class ProviderIndexSyntax(index: ProviderIndex) {
    def matchPhrase(op: Option[String]): Seq[Ranked[Provider]] = op match {
      case None => index.all
      case Some(phrase) => index.matchPhrase(phrase)
    }
  }

  def search(phrase: Option[String], lato: Option[Double], lono: Option[Double], disto: Option[Double]) = JsCollect {
    val lsp = for (lat <- lato; lon <- lono; dist <- disto)
      yield LocationSearchParams(Point(lat, lon), dist)

    ProviderIndex.matchPhrase(phrase).searchLocation(lsp).sortBy(_.rank).map(Json.toJson(_).as[JsObject])
  }

  def apprenticeships(ukprn: UKPRN) = JsCollect {
    dataStore.apprenticeships.filter(_.provider_id == ukprn).sortBy(_.description).map(Json.toJson(_).as[JsObject])
  }

  def providers = JsCollect(dataStore.providersJs)
}
