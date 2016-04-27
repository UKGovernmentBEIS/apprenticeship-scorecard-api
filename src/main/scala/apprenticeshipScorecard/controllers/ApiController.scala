package apprenticeshipScorecard.controllers

import javax.inject.Inject

import apprenticeshipScorecard.models._
import apprenticeshipScorecard.queries.QueryAST.Query
import apprenticeshipScorecard.tools.TSVLoader
import play.api.libs.json._
import play.api.mvc.{Action, Controller}

import scala.concurrent.ExecutionContext

class ApiController @Inject()(implicit ec: ExecutionContext) extends Controller {
  implicit val providerFormat = Json.format[Provider]
  implicit val apprenticeshipFormat = Json.format[Apprenticeship]

  def providers(page_number: Option[Int], page_size: Option[Int], max_results: Option[Int], q: Option[Query]) = Action {

    val providers =
      TSVLoader.dataStore.providers.values.toSeq
        .query(q)
        .sortBy(_.name)
        .limit(max_results)

    val page = ResultsPage.build(providers, PageNumber(page_number.getOrElse(1)), max_results.getOrElse(Int.MaxValue), PageCount(page_size.getOrElse(50)))
    val results = SearchResults(page.resultsForPage, page.resultCount, page.currentPage.num, page.perPage.count)

    Ok(Json.toJson(results))
  }

  def apprenticeships(page_number: Option[Int], page_size: Option[Int], max_results: Option[Int], q: Option[Query]) = Action {
    val apprenticeships =
      TSVLoader.dataStore.apprenticeships
        .query(q)
        .sortBy(_.description)
        .limit(max_results)

    val page = ResultsPage.build(apprenticeships, PageNumber(page_number.getOrElse(1)), max_results.getOrElse(Int.MaxValue), PageCount(page_size.getOrElse(50)))
    val results = SearchResults(page.resultsForPage, page.resultCount, page.currentPage.num, page.perPage.count)

    Ok(Json.toJson(results))
  }
}
