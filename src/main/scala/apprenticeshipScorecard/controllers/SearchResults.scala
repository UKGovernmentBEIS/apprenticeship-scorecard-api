package apprenticeshipScorecard.controllers

import play.api.libs.json._

case class SearchResults[T: Writes](results: Seq[T], result_count: Int, page_number: Int, page_size: Int)

object SearchResults {
  implicit def formats[T: Writes] = new Writes[SearchResults[T]] {
    override def writes(o: SearchResults[T]): JsValue = {
      implicit val w1 = play.api.libs.json.Writes.traversableWrites
      implicit val tw = implicitly[Writes[T]]
      val resultsJson = Json.toJson(o.results)
      JsObject(Map(
        "results" -> resultsJson,
        "result_count" -> JsNumber(o.result_count),
        "page_number" -> JsNumber(o.page_number),
        "page_size" -> JsNumber(o.page_size)
      ))
    }
  }
}
