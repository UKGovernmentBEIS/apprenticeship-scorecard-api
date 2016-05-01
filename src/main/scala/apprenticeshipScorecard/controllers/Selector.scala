package apprenticeshipScorecard.controllers

import apprenticeshipScorecard.bindings
import apprenticeshipScorecard.tools.{Subject, TSVLoader}
import com.wellfactored.restless.QueryAST.{Path, Query}
import play.api.libs.json.{JsResult, _}

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
                     q: Option[Query],
                     extract: Option[List[Path]])

  implicit val paramsR = Json.reads[Params]


  implicit class Select[T: Writes](ts: Seq[T]) {
    def select[B](params: Params)(sortKey: (T) => B)(implicit ordering: Ordering[B]): SearchResults[JsObject] = {
      import params._

      val results = selectJson(ts, params.q, params.extract, params.max_results)(sortKey)
      val page = ResultsPage.build(results, PageNumber(page_number.getOrElse(1)), max_results.getOrElse(Int.MaxValue), PageCount(page_size.getOrElse(50)))
      SearchResults(page.resultsForPage, page.resultCount, page.currentPage.num, page.perPage.count)
    }
  }


  implicit val subjectW = Json.writes[Subject]

  def findSubjects(params: Params): SearchResults[Subject] = findSubjects(params.page_number, params.page_size, params.max_results, params.q)

  def findSubjects(page_number: Option[Int], page_size: Option[Int], max_results: Option[Int], q: Option[Query]): SearchResults[Subject] = {
    val subjects =
      TSVLoader.dataStore.subjects.values.toSeq
        .where(q)
        .sortBy(_.subject_tier_2_code.code)
        .limit(max_results)

    val page = ResultsPage.build(subjects, PageNumber(page_number.getOrElse(1)), max_results.getOrElse(Int.MaxValue), PageCount(page_size.getOrElse(50)))
    SearchResults(page.resultsForPage, page.resultCount, page.currentPage.num, page.perPage.count)
  }


}
