package apprenticeshipScorecard.controllers

import javax.inject.Inject

import apprenticeshipScorecard.models._
import apprenticeshipScorecard.tools.TSVLoader
import com.wellfactored.restless.QueryAST.Query
import play.api.libs.json._
import play.api.mvc.{Action, Controller}

import scala.concurrent.ExecutionContext

class ApiController @Inject()(implicit ec: ExecutionContext) extends Controller {

  import Selector._

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

  def providers(page_number: Option[Int], page_size: Option[Int], max_results: Option[Int], qo: Option[Query]) = Action {
    val params = Params(page_number, page_size, max_results, qo, None)
    val results = dataStore.providers.values.toSeq.select(params, new JsonIdentity[Provider].project)(_.name)

    Ok(Json.toJson(results))
  }

  def providersPost = Action(parse.json) { request =>
    request.body.validate[Params].fold(
      invalid => BadRequest("bad parameter format"),
      params => {
        val projectF: Provider => JsObject = params.extract.map { paths =>
          new JsonProjector[Provider](paths.map(_.names)).project(_)
        }.getOrElse(new JsonIdentity[Provider].project(_))

        Ok(Json.toJson(dataStore.providers.values.toList.select(params, projectF)(_.name)))
      }
    )
  }

  def apprenticeships(page_number: Option[Int], page_size: Option[Int], max_results: Option[Int], q: Option[Query]) = Action {
    val params = Params(page_number, page_size, max_results, q, None)

    Ok(Json.toJson(dataStore.apprenticeships.select(params, new JsonIdentity[Apprenticeship].project)(_.description)))
  }

  def apprenticeshipsPost = Action(parse.json) { request =>
    request.body.validate[Params].fold(
      invalid => BadRequest("bad parameter format"),
      params => Ok(Json.toJson(dataStore.apprenticeships.select(params, new JsonIdentity[Apprenticeship].project)(_.description)))
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
