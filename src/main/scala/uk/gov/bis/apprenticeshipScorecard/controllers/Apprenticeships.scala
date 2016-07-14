package uk.gov.bis.apprenticeshipScorecard.controllers

import javax.inject.Inject

import com.wellfactored.restless.play.actions.ApiActions.JsCollect
import play.api.libs.json.{JsObject, Json}
import play.api.mvc._
import uk.gov.bis.apprenticeshipScorecard.tools.DataStore.ApprenticeshipWithProvider
import uk.gov.bis.apprenticeshipScorecard.tools.{ApprenticeshipIndex, Ranked, TSVLoader}

import scala.concurrent.ExecutionContext

class Apprenticeships @Inject()(implicit ec: ExecutionContext) extends Controller with LocationSearchSupport {
  def apprenticeships = JsCollect(TSVLoader.dataStore.apprenticeshipsJs)

  implicit class ApprenticeshipIndexSyntax(index: ApprenticeshipIndex) {
    def matchPhrase(op: Option[String]): Seq[Ranked[ApprenticeshipWithProvider]] = op match {
      case None => index.all
      case Some(phrase) => index.matchPhrase(phrase)
    }
  }

  implicit object LocatableA extends Locatable[ApprenticeshipWithProvider] {
    override def lat(t: ApprenticeshipWithProvider): Option[BigDecimal] = t.secondary.address.latitude

    override def lon(t: ApprenticeshipWithProvider): Option[BigDecimal] = t.secondary.address.longitude
  }

  def search(phrase: Option[String], lato: Option[Double], lono: Option[Double], disto: Option[Double]) = JsCollect {
    val lsp = for (lat <- lato; lon <- lono; dist <- disto)
      yield LocationSearchParams(Point(lat, lon), dist)

    ApprenticeshipIndex.matchPhrase(phrase).searchLocation(lsp).sortBy(_.rank).map(Json.toJson(_).as[JsObject])
  }
}
