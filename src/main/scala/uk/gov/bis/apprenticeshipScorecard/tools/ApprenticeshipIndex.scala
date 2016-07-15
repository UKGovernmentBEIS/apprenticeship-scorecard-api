package uk.gov.bis.apprenticeshipScorecard.tools

import uk.gov.bis.apprenticeshipScorecard.models.{Apprenticeship, SubjectCode}
import uk.gov.bis.apprenticeshipScorecard.tools.DataStore.ApprenticeshipWithProvider

class ApprenticeshipIndex(dataStore: DataStore) extends Index[ApprenticeshipWithProvider] {

  override def all: Seq[Ranked[ApprenticeshipWithProvider]] =
    dataStore.apprenticeshipsWithProvider.map(Ranked(_, 1)).toSeq

  override def lookup(word: String): Seq[Ranked[ApprenticeshipWithProvider]] = {
    val (keywordMap, codeMap) = extractWordIndices match {
      case (keywordMaps, subjectCodeMaps) => (mergeMaps(keywordMaps), mergeMaps(subjectCodeMaps))
    }
    lookupWord(word, keywordMap, codeMap)
  }

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