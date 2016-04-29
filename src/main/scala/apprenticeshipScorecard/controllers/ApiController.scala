package apprenticeshipScorecard.controllers

import javax.inject.Inject

import apprenticeshipScorecard.models._
import apprenticeshipScorecard.tools.TSVLoader
import com.wellfactored.restless.QueryAST.Query
import play.api.libs.json._
import play.api.mvc.{Action, Controller}

import scala.concurrent.ExecutionContext

class ApiController @Inject()(implicit ec: ExecutionContext) extends Controller {

  import Finder._

  implicit val providerFormat = Json.format[Provider]
  implicit val apprenticeshipFormat = Json.format[Apprenticeship]

  def provider(ukprn: Long) = Action { implicit request =>
    TSVLoader.dataStore.providers.get(UKPRN(ukprn)) match {
      case None => NotFound
      case Some(p) => Ok(Json.toJson(p))
    }
  }

  def subject(subjectCode: BigDecimal) = Action { implicit request =>
    TSVLoader.dataStore.subjects.get(SubjectCode(subjectCode)) match {
      case None => NotFound
      case Some(p) => Ok(Json.toJson(p))
    }
  }

  import TSVLoader.dataStore

  def providers(page_number: Option[Int], page_size: Option[Int], max_results: Option[Int], q: Option[Query]) = Action {
    val params = Params(page_number, page_size, max_results, q)
    Ok(Json.toJson(dataStore.providers.values.toList.select(params, new IdProjection[Provider])(_.name)))
  }

  def providersPost = Action(parse.json) { request =>
    request.body.validate[Params].fold(
      invalid => BadRequest("bad parameter format"),
      params => Ok(Json.toJson(dataStore.providers.values.toList.select(params, new IdProjection[Provider])(_.name)))
    )
  }


  def apprenticeships(page_number: Option[Int], page_size: Option[Int], max_results: Option[Int], q: Option[Query]) = Action {
    val params = Params(page_number, page_size, max_results, q)

    Ok(Json.toJson(dataStore.apprenticeships.select(params, new IdProjection[Apprenticeship])(_.description)))
  }

  def apprenticeshipsPost = Action(parse.json) { request =>
    request.body.validate[Params].fold(
      invalid => BadRequest("bad parameter format"),
      params => Ok(Json.toJson(dataStore.apprenticeships.select(params, new IdProjection[Apprenticeship])(_.description)))
    )
  }

  def subjects(page_number: Option[Int], page_size: Option[Int], max_results: Option[Int], q: Option[Query]) = Action {
    Ok(Json.toJson(findSubjects(page_number, page_size, max_results, q)))
  }

  def subjectsPost = Action(parse.json) { request =>
    request.body.validate[Params].fold(
      invalid => BadRequest("bad parameter format"),
      params => Ok(Json.toJson(findSubjects(params)))
    )
  }


}
