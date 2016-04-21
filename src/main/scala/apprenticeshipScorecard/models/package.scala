package apprenticeshipScorecard

package object models {

  case class UKPRN(id: Long) extends AnyVal

  case class Learner(
                      satisfaction: Option[BigDecimal],
                      nationalSatisfaction: Option[BigDecimal],
                      ageUnder19: Option[Int],
                      age19to24: Option[Int],
                      age25plus: Option[Int],
                      total: Option[Int])

  case class Earnings(median: Option[BigDecimal], proportionAbove21k: Option[BigDecimal])

  case class QualificationStats(successRate: Option[BigDecimal], retentionRate: Option[BigDecimal], achievementRate: Option[BigDecimal])

  case class Address(
                      address1: Option[String],
                      address2: Option[String],
                      town: Option[String],
                      county: Option[String],
                      postcode: Option[String],
                      latitude: Option[BigDecimal],
                      longitude: Option[BigDecimal]
                    )

  case class Provider(
                       ukprn: UKPRN,
                       name: String,
                       providerType: String,
                       region: String,
                       lea: String,
                       la: String,
                       address: Address,
                       website: Option[String])

  case class Apprenticeship(
                             subjectTier2Code: String,
                             subjectTier2Title: String,
                             learner: Learner,
                             description: String,
                             providerId: UKPRN,
                             stats: QualificationStats,
                             nationalStats: QualificationStats,
                             earnings: Earnings,
                             nationalEarnings: Earnings,
                             averageCost: BigDecimal)

}
