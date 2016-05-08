package uk.gov.bis.apprenticeshipScorecard.controllers

import com.wellfactored.restless.QueryAST.{Path, Query}
import play.api.libs.json._
import uk.gov.bis.apprenticeshipScorecard.bindings
import uk.gov.bis.apprenticeshipScorecard.tools.Subject

object Selector {

  import com.wellfactored.restless.play.json.Selection._

  implicit val queryR = bindings.queryR

  implicit val pathR = new Reads[Path] {
    override def reads(json: JsValue): JsResult[Path] = implicitly[Reads[String]].reads(json).flatMap { js =>
      JsSuccess(Path(js.split('.').toList), JsPath(List()))
    }
  }

  case class Params(
                     page_number: Option[Int],
                     page_size: Option[Int],
                     max_results: Option[Int],
                     query: Option[Query],
                     fields: Option[List[Path]])

  implicit val paramsR = Json.reads[Params]


  implicit class Select[T: Writes](ts: Iterable[T]) {
    def select[B](params: Params)(sortKey: (T) => B)(implicit ordering: Ordering[B]): SearchResults[JsValue] = {
      import params._

      val results = selectJson(ts, params.query, params.fields, params.max_results)(sortKey).toSeq
      val page = ResultsPage.build(results, PageNumber(page_number.getOrElse(1)), max_results.getOrElse(Int.MaxValue), PageCount(page_size.getOrElse(50)))
      SearchResults(page.resultsForPage, page.resultCount, page.currentPage.num, page.perPage.count)
    }
  }

  implicit val subjectW = Json.writes[Subject]

}
