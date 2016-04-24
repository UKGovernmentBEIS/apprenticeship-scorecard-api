import apprenticeshipScorecard.models._
import play.api.libs.json._

package object apprenticeshipScorecard {
  implicit val prnFormats = new Format[UKPRN] {
    override def writes(prn: UKPRN): JsValue = JsNumber(prn.id)

    override def reads(json: JsValue): JsResult[UKPRN] = implicitly[Reads[Long]].reads(json).map(UKPRN)
  }

  implicit val addressFormats = Json.format[Address]
  implicit val providerFormats = Json.format[Provider]

  implicit val learnerFormats = Json.format[LearnerStats]
  implicit val qsFormats = Json.format[QualificationStats]
  implicit val earningsFormats = Json.format[Earnings]
  implicit val apprenticeshipFormats = Json.format[Apprenticeship]
}
