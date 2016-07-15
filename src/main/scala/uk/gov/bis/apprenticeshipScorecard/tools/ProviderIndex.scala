package uk.gov.bis.apprenticeshipScorecard.tools

import uk.gov.bis.apprenticeshipScorecard.models.SubjectCode
import uk.gov.bis.apprenticeshipScorecard.tools.DataStore.ProviderWithApprenticeships

class ProviderIndex(dataStore: DataStore) extends Index[ProviderWithApprenticeships] {
  override def all: Seq[Ranked[ProviderWithApprenticeships]] = dataStore.providersWithApprenticeships.map(Ranked(_, 1)).toSeq

  /**
    * For each provider build one map that indexes it against keywords and one that indexes
    * it against the subject codes it offers.
    *
    * @return a pair of maps with the keyword index as the first element and the subject code index
    *         as the second element.
    */
  override def extractWordIndices: (Iterable[Map[String, ProviderWithApprenticeships]], Iterable[Map[String, ProviderWithApprenticeships]]) =
  dataStore.providersWithApprenticeships.map { provider =>
    val (subjectCodes, subjectTitles) = extractSubjects(provider)

    val keywords = (splitWords(provider.primary.name) ++ subjectTitles.flatMap(splitWords)).map(normalise)
    (Map(keywords.map(k => k -> provider): _*), Map(subjectCodes.map(sc => sc.code.toString -> provider): _*))
  }.unzip


  def extractSubjects(provider: ProviderWithApprenticeships): (List[SubjectCode], List[String]) = {
    provider.secondary.map { a =>
      (a.subject_tier_2_code, a.subject_tier_2_title)
    }.toList.distinct.unzip
  }
}

object ProviderIndex extends ProviderIndex(TSVLoader.dataStore)