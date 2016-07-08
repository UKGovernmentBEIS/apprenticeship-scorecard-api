package uk.gov.bis.apprenticeshipScorecard

import eu.timepit.refined.api.Refined
import eu.timepit.refined.numeric._
import eu.timepit.refined._
import eu.timepit.refined.boolean._

package object models {

  case class UKPRN(id: Long) extends AnyVal

  type NonNegativeInt = Int Refined NonNegative
  type ZeroToOne = Double Refined Not[Less[W.`0.0`.T]] And Not[Greater[W.`1.0`.T]]

  case class SubjectCode(code: BigDecimal) extends AnyVal

  case class LearnerStats(
                           satisfaction: Option[BigDecimal],
                           national_satisfaction: Option[BigDecimal],
                           age_under_19: Option[NonNegativeInt],
                           age_19_to_24: Option[NonNegativeInt],
                           age_25_plus: Option[NonNegativeInt],
                           total: Option[NonNegativeInt])

  case class Earnings(median: Option[BigDecimal], proportion_above_21k: Option[BigDecimal])

  case class QualificationStats(success_rate: Option[BigDecimal], retention_rate: Option[BigDecimal], achievement_rate: Option[BigDecimal])

  case class Address(
                      address1: Option[String],
                      address2: Option[String],
                      town: Option[String],
                      county: Option[String],
                      post_code: Option[String],
                      latitude: Option[BigDecimal],
                      longitude: Option[BigDecimal]
                    )

  case class Provider(
                       ukprn: UKPRN,
                       name: String,
                       provider_type: String,
                       region: String,
                       lea: String,
                       la: String,
                       address: Address,
                       website: Option[String])

  case class Apprenticeship(
                             subject_tier_2_code: SubjectCode,
                             subject_tier_2_title: String,
                             learner_stats: LearnerStats,
                             description: String,
                             provider_id: UKPRN,
                             stats: QualificationStats,
                             national_stats: QualificationStats,
                             earnings: Earnings,
                             national_earnings: Earnings,
                             average_cost: BigDecimal)

}
