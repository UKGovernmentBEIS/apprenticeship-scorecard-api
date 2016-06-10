package uk.gov.bis.apprenticeshipScorecard.controllers

import javax.inject.Inject

import play.api.mvc._
import uk.gov.bis.apprenticeshipScorecard.tools.TSVLoader
import com.wellfactored.restless.play.actions.ApiActions.JsCollect

import scala.concurrent.ExecutionContext

class Apprenticeships @Inject()(implicit ec: ExecutionContext) extends Controller {
  def apprenticehships = JsCollect(TSVLoader.dataStore.apprenticeshipsJs)
}
