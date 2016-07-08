package uk.gov.bis

import com.wellfactored.playbindings.ValueClassFormats
import eu.timepit.refined._
import eu.timepit.refined.api.{Refined, Validate}
import play.api.libs.json._
import uk.gov.bis.apprenticeshipScorecard.models._
import uk.gov.bis.apprenticeshipScorecard.tools.Subject

package object apprenticeshipScorecard extends ValueClassFormats {
  implicit def refinedFormat[T: Format, P](implicit v: Validate[T, P]): Format[T Refined P] = new Format[T Refined P] {
    override def writes(o: Refined[T, P]): JsValue = implicitly[Format[T]].writes(o.get)

    override def reads(json: JsValue): JsResult[Refined[T, P]] = implicitly[Format[T]].reads(json).flatMap { t =>
      refineV[P](t) match {
        case Right(value) => JsSuccess(value)
        case Left(msg) => JsError(msg)
      }
    }
  }


  implicit val subjectFormats = Json.format[Subject]

  implicit val addressFormats = Json.format[Address]
  implicit val providerFormats = Json.format[Provider]

  implicit val learnerFormats = Json.format[LearnerStats]
  implicit val qsFormats = Json.format[QualificationStats]
  implicit val earningsFormats = Json.format[Earnings]
  implicit val apprenticeshipFormats = Json.format[Apprenticeship]
}
