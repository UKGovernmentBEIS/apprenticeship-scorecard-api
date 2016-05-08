package uk.gov.bis

import uk.gov.bis.apprenticeshipScorecard.models._
import play.api.libs.json._
import uk.gov.bis.apprenticeshipScorecard.tools.Subject

package object apprenticeshipScorecard {
  implicit val prnFormats = new Format[UKPRN] {
    override def writes(prn: UKPRN): JsValue = JsNumber(prn.id)

    override def reads(json: JsValue): JsResult[UKPRN] = implicitly[Reads[Long]].reads(json).map(UKPRN)
  }

  implicit val subjectCodeFormats = new Format[SubjectCode] {
    override def writes(code: SubjectCode): JsValue = JsNumber(code.code)

    override def reads(json: JsValue): JsResult[SubjectCode] = implicitly[Reads[BigDecimal]].reads(json).map(SubjectCode)
  }

  implicit val subjectFormats = Json.format[Subject]

  implicit val addressFormats = Json.format[Address]
  implicit val providerFormats = Json.format[Provider]

  implicit val learnerFormats = Json.format[LearnerStats]
  implicit val qsFormats = Json.format[QualificationStats]
  implicit val earningsFormats = Json.format[Earnings]
  implicit val apprenticeshipFormats = Json.format[Apprenticeship]
}
