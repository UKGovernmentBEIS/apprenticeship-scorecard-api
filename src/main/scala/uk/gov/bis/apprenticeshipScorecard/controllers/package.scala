package uk.gov.bis.apprenticeshipScorecard

import play.api.libs.json._
import play.api.mvc.Result
import uk.gov.bis.apprenticeshipScorecard.controllers.Locatable
import uk.gov.bis.apprenticeshipScorecard.models.{Apprenticeship, Provider}
import uk.gov.bis.apprenticeshipScorecard.tools.DataStore._
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

  /**
    * Unwrap the object from the rank, convert it to json and then inject
    * the `search_rank` and `distance` as a top-level properties.
    */
  implicit def formats[T: Writes] = new Writes[Ranked[T]] {
    override def writes(o: Ranked[T]): JsValue = {
      val resultsJson = Json.toJson(o.item).as[JsObject]
      val extras = Seq(
        Some("search_rank" -> JsNumber(o.rank)),
        o.distance.map(d => "distance" -> JsNumber(d))
      ).flatten

      JsObject(extras).++(resultsJson)
    }
  }

  implicit object locatableP extends Locatable[ProviderWithApprenticeships] {
    override def location(t: ProviderWithApprenticeships): Option[Point] = for {
      lat <- t.primary.address.latitude
      lon <- t.primary.address.longitude
    } yield Point(lat.doubleValue(), lon.doubleValue())
  }

  implicit object locatableA extends Locatable[ApprenticeshipWithProvider] {
    override def location(t: ApprenticeshipWithProvider): Option[Point] = for {
      lat <- t.secondary.address.latitude
      lon <- t.secondary.address.longitude
    } yield Point(lat.doubleValue(), lon.doubleValue())
  }
}