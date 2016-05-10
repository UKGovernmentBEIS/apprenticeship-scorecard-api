package uk.gov.bis.apprenticeshipScorecard.controllers

import javax.inject.Inject

import play.api.libs.json._
import play.api.mvc._
import uk.gov.bis.apprenticeshipScorecard.ApiActions.Collect
import uk.gov.bis.apprenticeshipScorecard.tools.TSVLoader

import scala.concurrent.ExecutionContext

class Apprenticeships @Inject()(implicit ec: ExecutionContext) extends Controller {
  def apprenticehships = Collect(TSVLoader.dataStore.apprenticeships)(_.description)
}
