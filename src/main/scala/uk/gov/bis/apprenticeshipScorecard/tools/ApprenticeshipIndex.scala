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
      // Not the most efficient implementation, but good enough for the 800-odd providers in our data set
      override def lookup(word: String): Seq[Ranked[ApprenticeshipWithProvider]] = {
        val searchWord = word.trim().toLowerCase
        if (searchWord != "") {
          val wordMatches = keywordMap.keys.filter(_.startsWith(searchWord)).flatMap { key =>
            keywordMap(key).map(prn => Ranked(prn, 1.0 + searchWord.length.toDouble / key.length))
          }
          val codeMatches = codeMap.keys.filter(_ == searchWord).flatMap(codeMap(_).map(prn => Ranked(prn, 2.0)))
          (wordMatches ++ codeMatches).toSeq
        } else Seq()
      }

      override def all: Seq[Ranked[ApprenticeshipWithProvider]] = dataStore.apprenticeshipsWithProvider.map(Ranked(_, 1)).toSeq
    }
  }

  def normalise(s: String): String = s.trim.toLowerCase

  def splitWords(s: String): List[String] = s.split("\\s").toList

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

  def mergeMaps(maps: Iterable[Map[String, ApprenticeshipWithProvider]]): Map[String, Set[ApprenticeshipWithProvider]] = {
    maps.foldLeft(Map[String, Set[ApprenticeshipWithProvider]]()) { case (alreadyMerged, stringToUkprn) =>
      alreadyMerged ++ stringToUkprn.map { case (k, v) => k -> alreadyMerged.get(k).map(_ + v).getOrElse(Set(v)) }
    }
  }
}

object ApprenticeshipIndex extends ApprenticeshipIndex(TSVLoader.dataStore)