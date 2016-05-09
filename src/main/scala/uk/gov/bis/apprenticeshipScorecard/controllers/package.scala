package uk.gov.bis.apprenticeshipScorecard

import play.api.libs.json._
import play.api.mvc.Result

package object controllers {

  import play.api.mvc.Results._

  def jsonAction[T: Reads, W: Writes](json: JsValue)(body: T => W): Result = {
    json.validate[T].fold(
      invalid => BadRequest("bad parameter format"),
      t => Ok(Json.toJson(body(t)))
    )
  }
}