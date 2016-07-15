package uk.gov.bis.apprenticeshipScorecard.tools

import uk.gov.bis.apprenticeshipScorecard.models.{Apprenticeship, SubjectCode}
import uk.gov.bis.apprenticeshipScorecard.tools.DataStore.ApprenticeshipWithProvider

class ApprenticeshipIndex(dataStore: DataStore) extends Index[ApprenticeshipWithProvider] {

  override def all: Seq[Ranked[ApprenticeshipWithProvider]] =
    dataStore.apprenticeshipsWithProvider.map(Ranked(_, 1)).toSeq

  /**
    * Match a single word against the index
    *
    * @param word the word to match
    * @return a set of entries with a matching rank
    */
  override def lookup(word: String): Seq[Ranked[ApprenticeshipWithProvider]] = index.lookup(word)

  lazy val index: Index[ApprenticeshipWithProvider] = {
    val (keywordMap, codeMap) = extractMaps

    new Index[ApprenticeshipWithProvider] {
      override def lookup(word: String): Seq[Ranked[ApprenticeshipWithProvider]] = lookupWord(word, keywordMap, codeMap)

      override def all: Seq[Ranked[ApprenticeshipWithProvider]] = dataStore.apprenticeshipsWithProvider.map(Ranked(_, 1)).toSeq
    }
  }

  def extractMaps = extractWordIndices match {
    case (keywordMaps, subjectCodeMaps) => (mergeMaps(keywordMaps), mergeMaps(subjectCodeMaps))
  }

  /**
    * For each provider build one map that indexes it against keywords and one that indexes
    * it against the subject codes it offers.
    *
    * @return a pair of maps with the keyword index as the first element and the subject code index
    *         as the second element.
    */
  def extractWordIndices: (Iterable[Map[String, ApprenticeshipWithProvider]], Iterable[Map[String, ApprenticeshipWithProvider]]) =
  dataStore.apprenticeshipsWithProvider.map { app =>
    val (subjectCode, subjectTitle) = extractSubject(app.primary)
    val provider = app.secondary

    val keywords = (splitWords(provider.name) ++ splitWords(subjectTitle)).map(normalise)
    (Map(keywords.map(k => k -> app): _*), Map(subjectCode.code.toString -> app))
  }.unzip


  def extractSubject(app: Apprenticeship): (SubjectCode, String) = (app.subject_tier_2_code, app.subject_tier_2_title)


}

object ApprenticeshipIndex extends ApprenticeshipIndex(TSVLoader.dataStore)