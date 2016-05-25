package uk.gov.bis.apprenticeshipScorecard.tools

import uk.gov.bis.apprenticeshipScorecard.models.{Provider, UKPRN}

trait Index[T] {
  /**
    * Match a single word against the index
    *
    * @param word the word to match
    * @return a set of matching entries
    */
  def lookup(word: String): Set[T]

  def matchPhrase(phrase: String): Set[T] =
    phrase.trim.split("\\s").foldLeft(Set[T]()) { case (results, word) =>
      results ++ lookup(word)
    }
}

case class Ranked[T](item: T, rank: Double)

object ProviderIndex extends ProviderIndex(TSVLoader.dataStore)

class ProviderIndex(dataStore: DataStore) extends Index[Provider] {
  override def lookup(s: String): Set[Provider] = index.lookup(s).flatMap(prn => dataStore.providers.get(prn))

  lazy val index: Index[UKPRN] = {
    val (keywordMap, codeMap) = extractMaps

    new Index[UKPRN] {
      // Not the most efficient implementation, but good enough for the 800-odd providers in our data set
      override def lookup(word: String): Set[UKPRN] = {
        if (word.trim() != "") {
          val wordMatches = keywordMap.keys.filter(_.startsWith(word.trim())).flatMap(keywordMap(_))
          val codeMatches = codeMap.keys.filter(_ == word.trim()).flatMap(codeMap(_))
          (wordMatches ++ codeMatches).toSet
        } else Set()
      }
    }
  }

  def extractMaps = {
    val maps = dataStore.providers.values.map { provider =>
      val subjects = dataStore.apprenticeships.find(_.provider_id == provider.ukprn).map { a =>
        (a.subject_tier_2_code, a.subject_tier_2_title)
      }.toList.distinct

      val keywords = provider.name.split("\\s") ++ subjects.flatMap(_._2.split("\\s"))
      val codes = subjects.map(_._1)

      (Map(keywords.map(k => k -> provider.ukprn): _*), Map(codes.map(code => code.code.toString -> provider.ukprn): _*))
    }.toSeq

    (mergeMaps(maps.map(_._1)), mergeMaps(maps.map(_._2)))
  }

  def mergeMaps(maps: Seq[Map[String, UKPRN]]): Map[String, Set[UKPRN]] = {
    maps.foldLeft(Map[String, Set[UKPRN]]()) { case (alreadyMerged, stringToUkprn) =>
      alreadyMerged ++ stringToUkprn.map { case (k, v) => k -> alreadyMerged.get(k).map(_ + v).getOrElse(Set(v)) }
    }
  }


}