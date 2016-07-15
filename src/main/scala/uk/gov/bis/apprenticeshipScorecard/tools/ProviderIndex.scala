package uk.gov.bis.apprenticeshipScorecard.tools

import uk.gov.bis.apprenticeshipScorecard.models.{SubjectCode, UKPRN}
import uk.gov.bis.apprenticeshipScorecard.tools.DataStore.ProviderWithApprenticeships

class ProviderIndex(dataStore: DataStore) extends Index[ProviderWithApprenticeships] {
  override def lookup(s: String): Seq[Ranked[ProviderWithApprenticeships]] = for {
    rankedPrn <- index.lookup(s)
    provider <- dataStore.providersWithApprenticeships.find(_.primary.ukprn == rankedPrn.item)
  } yield Ranked(provider, rankedPrn.rank)

  override def all: Seq[Ranked[ProviderWithApprenticeships]] =
    dataStore.providersWithApprenticeships.map(Ranked(_, 1)).toSeq

  lazy val index: Index[UKPRN] = {
    val (keywordMap, codeMap) = extractMaps

    new Index[UKPRN] {
      override def lookup(word: String): Seq[Ranked[UKPRN]] = lookupWord(word, keywordMap, codeMap)

      override def all: Seq[Ranked[UKPRN]] = dataStore.providers.keys.map(Ranked(_, 1)).toSeq
    }
  }

  def extractMaps: (Map[String, Set[UKPRN]], Map[String, Set[UKPRN]]) = extractWordIndices match {
    case (keywordMaps, subjectCodeMaps) => (mergeMaps(keywordMaps), mergeMaps(subjectCodeMaps))
  }

  /**
    * For each provider build one map that indexes it against keywords and one that indexes
    * it against the subject codes it offers.
    *
    * @return a pair of maps with the keyword index as the first element and the subject code index
    *         as the second element.
    */
  def extractWordIndices: (Iterable[Map[String, UKPRN]], Iterable[Map[String, UKPRN]]) =
    dataStore.providersWithApprenticeships.map { join =>
      val (subjectCodes, subjectTitles) = extractSubjects(join)
      val provider = join.primary

      val keywords = (splitWords(provider.name) ++ subjectTitles.flatMap(splitWords)).map(normalise)
      (Map(keywords.map(k => k -> provider.ukprn): _*), Map(subjectCodes.map(sc => sc.code.toString -> provider.ukprn): _*))
    }.unzip


  def extractSubjects(provider: ProviderWithApprenticeships): (List[SubjectCode], List[String]) = {
    provider.secondary.map { a =>
      (a.subject_tier_2_code, a.subject_tier_2_title)
    }.toList.distinct.unzip
  }
}

object ProviderIndex extends ProviderIndex(TSVLoader.dataStore)