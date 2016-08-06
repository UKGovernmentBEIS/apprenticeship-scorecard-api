package uk.gov.bis.apprenticeshipScorecard.controllers

import javax.inject.Inject

import com.wellfactored.restless.play.actions.ApiActions._
import play.api.libs.json.Writes
import play.api.mvc._
import uk.gov.bis.apprenticeshipScorecard.models.{Apprenticeship, Join, Provider}
import uk.gov.bis.apprenticeshipScorecard.tools.DataStore.ApprenticeshipWithProvider
import uk.gov.bis.apprenticeshipScorecard.tools.{ApprenticeshipIndex, TSVLoader}

import scala.concurrent.ExecutionContext

class Apprenticeships @Inject()(implicit ec: ExecutionContext) extends Controller with SearchSupport[ApprenticeshipWithProvider] {
  def apprenticeships = JsCollect(TSVLoader.dataStore.apprenticeshipsJs)

  override def index = ApprenticeshipIndex

  override implicit def locator: Locatable[ApprenticeshipWithProvider] = locatableA

  override implicit def writes: Writes[ApprenticeshipWithProvider] = Join.writes[Apprenticeship, Provider]
}
