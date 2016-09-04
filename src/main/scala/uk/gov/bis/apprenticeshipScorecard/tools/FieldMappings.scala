package uk.gov.bis.apprenticeshipScorecard.tools

sealed trait Binding

case class SimpleBinding(fieldName: String) extends Binding

case class StructureBinding(mapping: Map[Binding, String]) extends Binding

object FieldMappings {

  def field(name: String): Binding = SimpleBinding(name)

  def struct(mapping: Map[Binding, String]): Binding = StructureBinding(mapping)

  val learnerStatsMappings: Map[Binding, String] = Map(
    field("learner_satisfaction_score") -> "satisfaction",
    field("learner_satisfaction_score_avg") -> "national_satisfaction",
    field("learners_age_under_19") -> "age_under_19",
    field("learners_age_19") -> "age_19_to_24",
    field("learners_age_25_plus") -> "age_25_plus",
    field("learners_total") -> "total",
    field("apprentices_intermediate") -> "intermediate",
    field("apprentices_advanced") -> "advanced",
    field("apprentices_higher") -> "higher"
  )

  val apprenticeshipMappings: Map[Binding, String] = Map(
    field("ssa_tier_2_code") -> "subject_tier_2_code",
    struct(learnerStatsMappings) -> "learner_stats"
  )

  val providerMappings: Map[Binding, String] = Map(
    field("ukprn") -> "ukprn",
    field("provision_type") -> "provision_type",
    field("level") -> "level",
    field("provider_name") -> "name",
    field("provider_type") -> "provider_type",
    field("provider_region") -> "region",
    field("provider_lea") -> "lea",
    field("provider_la") -> "la",
    field("website") -> "website"
  )

}
