package uk.gov.bis.apprenticeshipScorecard.tools

import cats.data.ValidatedNel
import cats.syntax.cartesian._
import uk.gov.bis.apprenticeshipScorecard.models._
import uk.gov.bis.apprenticeshipScorecard.tools.FieldExtractors._

trait Extractor[T] {
  def extract(implicit fields: Map[String, String]): ValidatedNel[String, T]
}

object UKPRNExtractor extends Extractor[UKPRN] {

  val fieldName = "ukprn"

  override def extract(implicit fields: Map[String, String]): ValidatedNel[String, UKPRN] =
    mandatory[Long](fieldName).map(UKPRN(_))
}

object SubjectCodeExtractor extends Extractor[SubjectCode] {

  val codeFieldName = "ssa_tier_2_code"

  override def extract(implicit fields: Map[String, String]): ValidatedNel[String, SubjectCode] =
    mandatory[BigDecimal](codeFieldName).map(SubjectCode(_))
}

object LearnerExtractor extends Extractor[LearnerStats] {

  val lssFieldName = "learner_satisfaction_score"
  val lssaFieldName = "learner_satisfaction_score_avg"
  val ageU19FieldName = "learners_age_under_19"
  val age19to24FieldName = "learners_age_19-24"
  val age25plusFieldName = "learners_age_25_plus"
  val learnersTotalFieldName = "learners_total"
  val intermediateFieldName = "apprentices_intermediate"
  val advancedFieldName = "apprentices_advanced"
  val higherFieldName = "apprentices_higher"

  override def extract(implicit fields: Map[String, String]): ValidatedNel[String, LearnerStats] = {
    val s = optional[BigDecimal](lssFieldName).default(None)
    val ns = optional[BigDecimal](lssaFieldName).default(None)
    val u19 = optional[Int](ageU19FieldName).default(None)
    val u24 = optional[Int](age19to24FieldName).default(None)
    val plus = optional[Int](age25plusFieldName).default(None)
    val total = optional[Int](learnersTotalFieldName).default(None)
    val inter = optional[Int](intermediateFieldName)
    val advanced = optional[Int](advancedFieldName)
    val higher = optional[Int](higherFieldName)

    (s |@| ns |@| u19 |@| u24 |@| plus |@| total |@| inter |@| advanced |@| higher).map(LearnerStats.apply)
  }
}

case class EarningsExtractor(national: Boolean = false) extends Extractor[Earnings] {

  val suffix = if (national) "_avg" else ""
  val medianFieldName = "median_earnings" + suffix
  val above21KFieldName = "proportion_earning_above_21k" + suffix

  override def extract(implicit fields: Map[String, String]): ValidatedNel[String, Earnings] = {
    (optional[BigDecimal](medianFieldName) |@|
      optional[BigDecimal](above21KFieldName)
      ).map(Earnings.apply)
  }
}

case class QualificationStatsExtractor(national: Boolean = false) extends Extractor[QualificationStats] {

  val suffix = if (national) "_avg" else ""

  val passRateFieldName = "pass_rate" + suffix
  val retentionRateFieldName = "retention_rate" + suffix
  val achievementRateFieldName = "achievement_rate" + suffix

  override def extract(implicit fields: Map[String, String]): ValidatedNel[String, QualificationStats] = {
    val pr = optional[BigDecimal](passRateFieldName).default(None)
    val rr = optional[BigDecimal](retentionRateFieldName).default(None)
    val ar = optional[BigDecimal](achievementRateFieldName).default(None)

    (pr |@| rr |@| ar).map(QualificationStats.apply)
  }
}

object AddressExtractor extends Extractor[Address] {

  val addr1FieldName = "address_1"
  val addr2FieldName = "address_2"
  val townFieldName = "town"
  val countyFieldName = "county"
  val postCodeFieldName = "post_code"
  val latFieldName = "latitude"
  val lonFieldName = "longitude"

  override def extract(implicit fields: Map[String, String]): ValidatedNel[String, Address] = {
    val a1 = optional[String](addr1FieldName)
    val a2 = optional[String](addr2FieldName)
    val town = optional[String](townFieldName)
    val county = optional[String](countyFieldName)
    val postCode = optional[String](postCodeFieldName)
    val lat = optional[BigDecimal](latFieldName).default(None)
    val lon = optional[BigDecimal](lonFieldName).default(None)

    (a1 |@| a2 |@| town |@| county |@| postCode |@| lat |@| lon).map(Address.apply)
  }
}

object ProviderExtractor extends Extractor[Provider] {

  val provisionTypeFieldName = "provision_type"
  val levelFieldName = "level"
  val nameFieldName = "provider_name"
  val typeFieldName = "provider_type"
  val regionFieldName = "provider_region"
  val leaFieldName = "provider_lea"
  val laFieldName = "provider_la"
  val websiteFieldName = "website"


  override def extract(implicit fields: Map[String, String]): ValidatedNel[String, Provider] = {
    val ukprn = UKPRNExtractor.extract
    val provisionType = mandatory[String](provisionTypeFieldName)
    val level = mandatory[String](levelFieldName)
    val name = mandatory[String](nameFieldName)
    val typ = mandatory[String](typeFieldName)
    val region = mandatory[String](regionFieldName)
    val lea = mandatory[String](leaFieldName)
    val la = mandatory[String](laFieldName)
    val address = AddressExtractor.extract
    val web = optional[String](websiteFieldName)

    (ukprn |@| provisionType |@| level |@| name |@| typ |@| region |@| lea |@| la |@| address |@| web).map(Provider.apply)
  }
}

object ApprenticeshipExtractor extends Extractor[Apprenticeship] {

  val titleFieldName = "ssa_tier_2_description"
  val descriptionFieldName = "ssa_tier_2_description"
  val costFieldName = "average_cost"

  override def extract(implicit fields: Map[String, String]): ValidatedNel[String, Apprenticeship] = {
    val ukprn = UKPRNExtractor.extract
    val code = SubjectCodeExtractor.extract
    val title = mandatory[String](titleFieldName)
    val learner = LearnerExtractor.extract
    val desc = mandatory[String](descriptionFieldName)
    val stats = QualificationStatsExtractor(false).extract
    val natStats = QualificationStatsExtractor(true).extract
    val earnings = EarningsExtractor(false).extract
    val natEarn = EarningsExtractor(true).extract
    val cost = mandatory[BigDecimal](costFieldName)

    (code |@| title |@| learner |@| desc |@| ukprn |@| stats |@| natStats |@| earnings |@| natEarn |@| cost).map(Apprenticeship.apply)

  }
}

object RecordExtractor extends Extractor[(Provider, Apprenticeship)] {

  override def extract(implicit fields: Map[String, String]): ValidatedNel[String, (Provider, Apprenticeship)] = {
    (ProviderExtractor.extract |@| ApprenticeshipExtractor.extract).tupled
  }
}