package apprenticeshipScorecard.controllers

import javax.inject.Inject

import apprenticeshipScorecard.models._
import apprenticeshipScorecard.tools.TSVLoader
import com.wellfactored.restless.QueryAST.Query
import play.api.libs.json._
import play.api.mvc.{Action, Controller, Result}

import scala.concurrent.ExecutionContext

class ApiController @Inject()(implicit ec: ExecutionContext) extends Controller {

  import Selector._

  implicit val providerFormat = Json.format[Provider]
  implicit val apprenticeshipFormat = Json.format[Apprenticeship]

  import TSVLoader.dataStore

  def provider(ukprn: Long) = Action { implicit request =>
    dataStore.providers.get(UKPRN(ukprn)) match {
      case None => NotFound
      case Some(p) => Ok(Json.toJson(p))
    }
  }

  def subject(subjectCode: BigDecimal) = Action { implicit request =>
    dataStore.subjects.get(SubjectCode(subjectCode)) match {
      case None => NotFound
      case Some(p) => Ok(Json.toJson(p))
    }
  }

  def providersGET(page_number: Option[Int], page_size: Option[Int], max_results: Option[Int], qo: Option[Query]) = Action {
    val params = Params(page_number, page_size, max_results, qo, None)
    val results = dataStore.providers.values.select(params)(_.name)

    Ok(Json.toJson(results))
  }

  def providersPOST = Action(parse.json) { request =>
    jsonAction(request.body) { params: Params => dataStore.providers.values.select(params)(_.name) }
  }

  def apprenticeshipsGET(page_number: Option[Int], page_size: Option[Int], max_results: Option[Int], q: Option[Query]) = Action {
    val params = Params(page_number, page_size, max_results, q, None)

    Ok(Json.toJson(dataStore.apprenticeships.select(params)(_.description)))
  }

  def apprenticeshipsPOST = Action(parse.json) { request =>
    jsonAction(request.body) { params: Params => dataStore.apprenticeships.select(params)(_.description) }
  }

  def subjectsGET(page_number: Option[Int], page_size: Option[Int], max_results: Option[Int], q: Option[Query]) = Action {
    val params = Params(page_number, page_size, max_results, q, None)

    Ok(Json.toJson(dataStore.subjects.values.select(params)(_.subject_tier_2_code.code)))
  }

  def subjectsPOST = Action(parse.json) { request =>
    jsonAction(request.body) { params: Params => dataStore.subjects.values.select(params)(_.subject_tier_2_code.code) }
  }

  def jsonAction[T: Reads, W: Writes](json: JsValue)(body: T => W): Result = {
    json.validate[T].fold(
      invalid => BadRequest("bad parameter format"),
      t => Ok(Json.toJson(body(t)))
    )
  }
}
