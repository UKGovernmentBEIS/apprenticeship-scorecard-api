package apprenticeshipScorecard

package object models {

  case class UKPRN(id: Long) extends AnyVal

  case class Learner(
                      satisfaction: BigDecimal,
                      nationalSatisfaction: BigDecimal,
                      ageUnder19: Int,
                      age19to24: Int,
                      age25plus: Int,
                      total: Int)

  case class Earnings(median: BigDecimal, proportionAbove21k: BigDecimal)

  case class QualificationStats(successRate: BigDecimal, retentionRate: BigDecimal, achievementRate: BigDecimal)

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
                             provider: Provider,
                             stats: QualificationStats,
                             nationalStats: QualificationStats,
                             earnings: Earnings,
                             nationalEarnings: Earnings,
                             averageCost: BigDecimal)

}
