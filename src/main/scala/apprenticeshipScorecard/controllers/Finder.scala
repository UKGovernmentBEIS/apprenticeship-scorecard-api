package apprenticeshipScorecard.controllers

import apprenticeshipScorecard.bindings
import apprenticeshipScorecard.tools.{Subject, TSVLoader}
import com.wellfactored.restless.QueryAST.Query
import play.api.libs.json.{JsObject, Json, Writes}

object Finder {
  implicit val queryR = bindings.queryR
  implicit val extractorR = bindings.extractorR

  case class Params(
                     page_number: Option[Int],
                     page_size: Option[Int],
                     max_results: Option[Int],
                     q: Option[Query],
                     e: Option[JsonExtractor] = None)

  implicit val paramsR = Json.reads[Params]

  implicit class Finder[T: Writes](xs: Seq[T]) {
    def select[B](params: Params)(sortKey: (T) => B)(implicit ordering: Ordering[B]): SearchResults[JsObject] = {
      import params._
      val results: Seq[JsObject] = xs
        .query(q)
        .sortBy(sortKey)
        .limit(max_results)
        .extract(e)

      val page = ResultsPage.build(results, PageNumber(page_number.getOrElse(1)), max_results.getOrElse(Int.MaxValue), PageCount(page_size.getOrElse(50)))
      SearchResults(page.resultsForPage, page.resultCount, page.currentPage.num, page.perPage.count)
    }
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
