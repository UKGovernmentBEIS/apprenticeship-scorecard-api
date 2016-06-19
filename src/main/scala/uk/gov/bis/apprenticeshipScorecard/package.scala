package uk.gov.bis

import com.wellfactored.playbindings.ValueClassFormats
import play.api.libs.json._
import uk.gov.bis.apprenticeshipScorecard.models._
import uk.gov.bis.apprenticeshipScorecard.tools.Subject

package object apprenticeshipScorecard extends ValueClassFormats {
  implicit val subjectFormats = Json.format[Subject]

  implicit val addressFormats = Json.format[Address]
  implicit val providerFormats = Json.format[Provider]

  implicit val learnerFormats = Json.format[LearnerStats]
  implicit val qsFormats = Json.format[QualificationStats]
  implicit val earningsFormats = Json.format[Earnings]
  implicit val apprenticeshipFormats = Json.format[Apprenticeship]
}
