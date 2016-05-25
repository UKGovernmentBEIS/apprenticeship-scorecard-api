package uk.gov.bis.apprenticeshipScorecard.controllers

import javax.inject.Inject

import play.api.libs.json.Json
import play.api.mvc.{Action, Controller}
import com.wellfactored.restless.play.actions.ApiActions.Collect
import uk.gov.bis.apprenticeshipScorecard.models.{Apprenticeship, Provider, UKPRN}
import uk.gov.bis.apprenticeshipScorecard.tools.{ProviderIndex, Ranked, TSVLoader}

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

  def search(phrase: String) = Collect(ProviderIndex.matchPhrase(phrase))(p => -p.rank)

  def apprenticeships(ukprn: UKPRN) = Collect(dataStore.apprenticeships.filter(_.provider_id == ukprn))(_.description)

  def providers = Collect(dataStore.providers.values)(_.name)
}
