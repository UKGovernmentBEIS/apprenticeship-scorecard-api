package apprenticeshipScorecard.controllers

import javax.inject.Inject

import apprenticeshipScorecard.models.Provider
import apprenticeshipScorecard.tools.TSVLoader
import play.api.libs.json.Json
import play.api.mvc.{Action, Controller}

import scala.concurrent.ExecutionContext

case class ProviderResults(results: Seq[Provider], resultCount: Int, pageNumber: Int)

object ProviderResults {
  implicit val formats = Json.format[ProviderResults]
}

class ApiController @Inject()(implicit ec: ExecutionContext) extends Controller {
  implicit val providerFormat = Json.format[Provider]

  def providers(page_number: Option[Int], page_size: Option[Int], max_results: Option[Int]) = Action {
    val max = max_results.getOrElse(1000)

    val providers = TSVLoader.dataStore.providers.values.toSeq.sortBy(_.name).take(max)
    val page = ResultsPage.build(providers, PageNumber(page_number.getOrElse(1)), max, PageCount(page_size.getOrElse(50)))
    val results = ProviderResults(page.resultsForPage, page.resultCount, page.currentPage.num)

    Ok(Json.toJson(results))
  }

  def apprenticeships = Action {
    Ok(Json.toJson(TSVLoader.dataStore.apprenticeships))
  }

}
