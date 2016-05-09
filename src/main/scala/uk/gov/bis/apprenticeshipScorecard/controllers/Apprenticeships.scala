package uk.gov.bis.apprenticeshipScorecard.controllers

import javax.inject.Inject

import play.api.libs.json._
import play.api.mvc._
import uk.gov.bis.apprenticeshipScorecard.ApiActions.Collection
import uk.gov.bis.apprenticeshipScorecard.tools.TSVLoader

import scala.concurrent.ExecutionContext

class Apprenticeships @Inject()(implicit ec: ExecutionContext) extends Controller {

  import Selector._
  import TSVLoader.dataStore

  def apprenticehships = Collection { implicit request =>
    Ok(Json.toJson(dataStore.apprenticeships.select(request.params)(_.description)))
  }

}
