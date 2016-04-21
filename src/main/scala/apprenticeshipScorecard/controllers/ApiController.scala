package apprenticeshipScorecard.controllers

import javax.inject.Inject

import apprenticeshipScorecard.tools.TSVLoader
import play.api.libs.json.Json
import play.api.mvc.{Action, Controller}

import scala.concurrent.ExecutionContext

class ApiController @Inject()(implicit ec: ExecutionContext) extends Controller {

  def providers = Action {
    Ok(Json.toJson(TSVLoader.dataStore.providers.values))
  }

  def apprenticeships = Action {
    Ok(Json.toJson(TSVLoader.dataStore.apprenticeships))
  }

}
