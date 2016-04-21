package apprenticeshipScorecard.tools

import apprenticeshipScorecard.models._
import cats.data.ValidatedNel
import cats.syntax.cartesian._

trait Extractor[T] {
  def extract(implicit fields: Map[String, String]): ValidatedNel[String, T]
}

object UKPRNExtractor extends Extractor[UKPRN] {

  import FieldExtractors._

  val fieldName = "ukprn"

  override def extract(implicit fields: Map[String, String]): ValidatedNel[String, UKPRN] =
    mandatory[Long](fieldName).map(UKPRN(_))

}

object LearnerExtractor extends Extractor[Learner] {

  import FieldExtractors._

  val lssFieldName = "learner_satisfaction_score"
  val lssaFieldName = "learner_satisfaction_score_avg"
  val ageU19FieldName = "learners_age_under_19"
  val age19to24FieldName = "learners_age_19-24"
  val age25plusFieldName = "learners_age_25_plus"
  val learnersTotalFieldName = "learners_total"

  override def extract(implicit fields: Map[String, String]): ValidatedNel[String, Learner] = {
    val s = optional[BigDecimal](lssFieldName).default(None)
    val ns = optional[BigDecimal](lssaFieldName).default(None)
    val u19 = optional[Int](ageU19FieldName).default(None)
    val u24 = optional[Int](age19to24FieldName).default(None)
    val plus = optional[Int](age25plusFieldName).default(None)
    val total = optional[Int](learnersTotalFieldName).default(None)

    (s |@| ns |@| u19 |@| u24 |@| plus |@| total).map(Learner.apply)
  }
}

case class EarningsExtractor(national: Boolean = false) extends Extractor[Earnings] {

  import FieldExtractors._

  val suffix = if (national) "_avg" else ""
  val medianFieldName = "median_earnings" + suffix
  val above21KFieldName = "proportion_earned_above_21k" + suffix

  override def extract(implicit fields: Map[String, String]): ValidatedNel[String, Earnings] = {
    val m = optional[BigDecimal](medianFieldName)
    val above = optional[BigDecimal](above21KFieldName)

    (m |@| above).map(Earnings.apply)
  }
}

case class QualificationStatsExtractor(national: Boolean = false) extends Extractor[QualificationStats] {

  import FieldExtractors._

  val suffix = if (national) "_avg" else ""

  val successRateFieldName = "qual_success_rate" + suffix
  val retentionRateFieldName = "qual_retention_rate" + suffix
  val achievementRateFieldName = "qual_achievement_rate" + suffix

  override def extract(implicit fields: Map[String, String]): ValidatedNel[String, QualificationStats] = {
    val sr = optional[BigDecimal](successRateFieldName).default(None)
    val rr = optional[BigDecimal](retentionRateFieldName).default(None)
    val ar = optional[BigDecimal](achievementRateFieldName).default(None)

    (sr |@| rr |@| ar).map(QualificationStats.apply)
  }
}

object AddressExtractor extends Extractor[Address] {

  import FieldExtractors._

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

  import FieldExtractors._

  val nameFieldName = "provider_name"
  val typeFieldName = "provider_type"
  val regionFieldName = "provider_region"
  val leaFieldName = "provider_lea"
  val laFieldName = "provider_la"
  val websiteFieldName = "website"

  override def extract(implicit fields: Map[String, String]): ValidatedNel[String, Provider] = {
    val ukprn = UKPRNExtractor.extract
    val name = mandatory[String](nameFieldName)
    val typ = mandatory[String](typeFieldName)
    val region = mandatory[String](regionFieldName)
    val lea = mandatory[String](leaFieldName)
    val la = mandatory[String](laFieldName)
    val address = AddressExtractor.extract
    val web = optional[String](websiteFieldName)

    (ukprn |@| name |@| typ |@| region |@| lea |@| la |@| address |@| web).map(Provider.apply)
  }
}

object ApprenticeshipExtractor extends Extractor[Apprenticeship] {

  import FieldExtractors._

  val codeFieldName = "ssa_tier2_code"
  val titleFieldName = "ssa_tier2_description"
  val descriptionFieldName = "ssa_tier2_description"
  val costFieldName = "average_cost"

  override def extract(implicit fields: Map[String, String]): ValidatedNel[String, Apprenticeship] = {
    val ukprn = UKPRNExtractor.extract
    val code = mandatory[String](codeFieldName)
    val title = mandatory[String](titleFieldName)
    val learner = LearnerExtractor.extract
    val desc = mandatory[String](descriptionFieldName)
    val stats = new QualificationStatsExtractor(false).extract
    val natStats = new QualificationStatsExtractor(true).extract
    val earnings = new EarningsExtractor(false).extract
    val natEarn = new EarningsExtractor(true).extract
    val cost = mandatory[BigDecimal](costFieldName)

    (code |@| title |@| learner |@| desc |@| ukprn |@| stats |@| natStats |@| earnings |@| natEarn |@| cost).map(Apprenticeship.apply)

  }
}

object RecordExtractor extends Extractor[(Provider, Apprenticeship)] {

  import FieldExtractors._

  override def extract(implicit fields: Map[String, String]): ValidatedNel[String, (Provider, Apprenticeship)] = {
    (ProviderExtractor.extract |@| ApprenticeshipExtractor.extract).tupled
  }
}