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
    val s = mandatory[BigDecimal](lssFieldName)
    val ns = mandatory[BigDecimal](lssaFieldName)
    val u19 = mandatory[Int](ageU19FieldName)
    val u24 = mandatory[Int](age19to24FieldName)
    val plus = mandatory[Int](age25plusFieldName)
    val total = mandatory[Int](learnersTotalFieldName)

    (s |@| ns |@| u19 |@| u24 |@| plus |@| total).map(Learner.apply)
  }
}

case class EarningsExtractor(national: Boolean = false) extends Extractor[Earnings] {

  import FieldExtractors._

  val suffix = if (national) "_avg" else ""
  val medianFieldName = "median_earnings" + suffix
  val above21KFieldName = "proportion_earned_above_21k" + suffix

  override def extract(implicit fields: Map[String, String]): ValidatedNel[String, Earnings] = {
    val m = mandatory[BigDecimal](medianFieldName)
    val above = mandatory[BigDecimal](above21KFieldName)

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
    val sr = mandatory[BigDecimal](successRateFieldName)
    val rr = mandatory[BigDecimal](retentionRateFieldName)
    val ar = mandatory[BigDecimal](achievementRateFieldName)

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