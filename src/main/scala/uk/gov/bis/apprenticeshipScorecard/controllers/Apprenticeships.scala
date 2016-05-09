package uk.gov.bis.apprenticeshipScorecard.controllers

import javax.inject.Inject

import com.wellfactored.restless.QueryAST.Query
import play.api.libs.json._
import play.api.mvc.{Action, Controller}
import uk.gov.bis.apprenticeshipScorecard.models._
import uk.gov.bis.apprenticeshipScorecard.tools.TSVLoader

import scala.concurrent.ExecutionContext

class Apprenticeships @Inject()(implicit ec: ExecutionContext) extends Controller {

  import Selector._

  implicit val providerFormat = Json.format[Provider]
  implicit val apprenticeshipFormat = Json.format[Apprenticeship]

  import TSVLoader.dataStore

  def get(page_number: Option[Int], page_size: Option[Int], max_results: Option[Int], q: Option[Query]) = Action {
    val params = Params(page_number, page_size, max_results, q, None)

    Ok(Json.toJson(dataStore.apprenticeships.select(params)(_.description)))
  }

  def post = Action(parse.json) { request =>
    jsonAction(request.body) { params: Params => dataStore.apprenticeships.select(params)(_.description) }
  }
}
