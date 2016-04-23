package apprenticeshipScorecard.controllers

import javax.inject.Inject

import apprenticeshipScorecard.tools.TSVLoader
import play.api.Logger
import play.api.libs.json.Json
import play.api.mvc.{Action, Controller}

import scala.concurrent.ExecutionContext

class ApiController @Inject()(implicit ec: ExecutionContext) extends Controller {

  def providers(limit: Option[Int]) = Action {
    val json = Json.toJson(TSVLoader.dataStore.providers.values.take(limit.map(_.min(100)).getOrElse(100)))
    Logger.debug("built json for providers")
    Ok(json)
  }

  def apprenticeships = Action {
    Ok(Json.toJson(TSVLoader.dataStore.apprenticeships))
  }

}
