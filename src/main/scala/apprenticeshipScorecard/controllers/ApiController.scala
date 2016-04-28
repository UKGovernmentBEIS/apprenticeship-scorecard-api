package apprenticeshipScorecard.controllers

import javax.inject.Inject

import apprenticeshipScorecard.bindings
import apprenticeshipScorecard.models._
import apprenticeshipScorecard.tools.{Subject, TSVLoader}
import com.wellfactored.restless.QueryAST.Query
import play.api.libs.json._
import play.api.mvc.{Action, Controller}

import scala.concurrent.ExecutionContext

class ApiController @Inject()(implicit ec: ExecutionContext) extends Controller {
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

  def providers(page_number: Option[Int], page_size: Option[Int], max_results: Option[Int], q: Option[Query]) = Action {
    Ok(Json.toJson(findProviders(page_number, page_size, max_results, q)))
  }

  def providersPost = Action(parse.json) { request =>
    request.body.validate[Params].fold(
      invalid => BadRequest("bad parameter format"),
      params => Ok(Json.toJson(findProviders(params)))
    )
  }

  def apprenticeships(page_number: Option[Int], page_size: Option[Int], max_results: Option[Int], q: Option[Query]) = Action {
    Ok(Json.toJson(findApprenticeships(page_number, page_size, max_results, q)))
  }

  def apprenticeshipsPost = Action(parse.json) { request =>
    request.body.validate[Params].fold(
      invalid => BadRequest("bad parameter format"),
      params => Ok(Json.toJson(findApprenticeships(params)))
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


  implicit val queryR = bindings.queryR

  case class Params(page_number: Option[Int], page_size: Option[Int], max_results: Option[Int], q: Option[Query])

  implicit val paramsR = Json.reads[Params]

  def findApprenticeships(params: Params): SearchResults[Apprenticeship] = findApprenticeships(params.page_number, params.page_size, params.max_results, params.q)

  def findApprenticeships(page_number: Option[Int], page_size: Option[Int], max_results: Option[Int], q: Option[Query]): SearchResults[Apprenticeship] = {
    val apprenticeships =
      TSVLoader.dataStore.apprenticeships
        .query(q)
        .sortBy(_.description)
        .limit(max_results)

    val page = ResultsPage.build(apprenticeships, PageNumber(page_number.getOrElse(1)), max_results.getOrElse(Int.MaxValue), PageCount(page_size.getOrElse(50)))
    SearchResults(page.resultsForPage, page.resultCount, page.currentPage.num, page.perPage.count)
  }

  def findProviders(params: Params): SearchResults[Provider] = findProviders(params.page_number, params.page_size, params.max_results, params.q)

  def findProviders(page_number: Option[Int], page_size: Option[Int], max_results: Option[Int], q: Option[Query]): SearchResults[Provider] = {
    val providers =
      TSVLoader.dataStore.providers.values.toList
        .query(q)
        .sortBy(_.name)
        .limit(max_results)

    val page = ResultsPage.build(providers, PageNumber(page_number.getOrElse(1)), max_results.getOrElse(Int.MaxValue), PageCount(page_size.getOrElse(50)))
    SearchResults(page.resultsForPage, page.resultCount, page.currentPage.num, page.perPage.count)
  }

  implicit val subjectW = Json.writes[Subject]

  def findSubjects(params: Params): SearchResults[Subject] = findSubjects(params.page_number, params.page_size, params.max_results, params.q)

  def findSubjects(page_number: Option[Int], page_size: Option[Int], max_results: Option[Int], q: Option[Query]): SearchResults[Subject] = {
    val subjects =
      TSVLoader.dataStore.subjects.values.toList
        .query(q)
        .sortBy(_.subject_tier_2_code.code)
        .limit(max_results)

    val page = ResultsPage.build(subjects, PageNumber(page_number.getOrElse(1)), max_results.getOrElse(Int.MaxValue), PageCount(page_size.getOrElse(50)))
    SearchResults(page.resultsForPage, page.resultCount, page.currentPage.num, page.perPage.count)
  }


}
