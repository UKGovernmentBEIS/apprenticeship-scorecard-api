package uk.gov.bis.apprenticeshipScorecard

import play.api.libs.json._
import play.api.mvc.Result
import uk.gov.bis.apprenticeshipScorecard.models.{Apprenticeship, Provider}
import uk.gov.bis.apprenticeshipScorecard.tools.Ranked

package object controllers {

  import play.api.mvc.Results._

  implicit val providerFormat = Json.format[Provider]
  implicit val apprenticeshipFormat = Json.format[Apprenticeship]

  def jsonAction[T: Reads, W: Writes](json: JsValue)(body: T => W): Result = {
    json.validate[T].fold(
      invalid => BadRequest("bad parameter format"),
      t => Ok(Json.toJson(body(t)))
    )
  }

  implicit def formats[T: Writes] = new Writes[Ranked[T]] {
    override def writes(o: Ranked[T]): JsValue = {
      val resultsJson = Json.toJson(o.item).as[JsObject]
      resultsJson.+("search_rank" -> JsNumber(o.rank))
    }
  }
}