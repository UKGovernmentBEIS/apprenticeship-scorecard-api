package uk.gov.bis.apprenticeshipScorecard.controllers

import javax.inject.Inject

import com.wellfactored.restless.play.actions.ApiActions._
import play.api.libs.json.{JsObject, Json, Writes}
import play.api.mvc.{Action, Controller}
import uk.gov.bis.apprenticeshipScorecard.models._
import uk.gov.bis.apprenticeshipScorecard.tools.DataStore._
import uk.gov.bis.apprenticeshipScorecard.tools.{ProviderIndex, TSVLoader}

import scala.concurrent.ExecutionContext

class Providers @Inject()(implicit ec: ExecutionContext) extends Controller with SearchSupport[ProviderWithApprenticeships] with OptionResults {

  implicit val providerFormat = Json.format[Provider]
  implicit val apprenticeshipFormat = Json.format[Apprenticeship]

  import TSVLoader.dataStore

  def find(ukprn: UKPRN) = Action { implicit request =>
    dataStore.providers.get(ukprn).map(Json.toJson(_)).toResult
  }

  override def index = ProviderIndex

  def apprenticeships(ukprn: UKPRN) = JsCollect {
    dataStore.apprenticeships.filter(_.provider_id == ukprn).sortBy(_.description).map(Json.toJson(_).as[JsObject])
  }

  def providers = JsCollect(dataStore.providersJs)

  override implicit def locator: Locatable[ProviderWithApprenticeships] = locatableP
  override implicit def writes: Writes[ProviderWithApprenticeships] = Join.writes[Provider, Seq[Apprenticeship]]
}
