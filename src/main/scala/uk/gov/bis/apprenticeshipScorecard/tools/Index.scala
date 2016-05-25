package uk.gov.bis.apprenticeshipScorecard.tools

import uk.gov.bis.apprenticeshipScorecard.models.{Provider, UKPRN}

trait Index[T] {
  /**
    * Match a single word against the index
    *
    * @param word the word to match
    * @return a set of entries with a matching rank
    */
  def lookup(word: String): Seq[Ranked[T]]

  def matchPhrase(phrase: String): Seq[Ranked[T]] = for {
    (k, vs) <- phrase.trim.split("\\s").toList.flatMap(lookup).groupBy(_.item).toSeq
  } yield Ranked(k, vs.map(_.rank).sum)
}

case class Ranked[T](item: T, rank: Double)

object ProviderIndex extends ProviderIndex(TSVLoader.dataStore)

class ProviderIndex(dataStore: DataStore) extends Index[Provider] {
  override def lookup(s: String): Seq[Ranked[Provider]] = for {
    rankedPrn <- index.lookup(s)
    provider <- dataStore.providers.get(rankedPrn.item)
  } yield Ranked(provider, rankedPrn.rank)

  lazy val index: Index[UKPRN] = {
    val (keywordMap, codeMap) = extractMaps

    new Index[UKPRN] {
      // Not the most efficient implementation, but good enough for the 800-odd providers in our data set
      override def lookup(word: String): Seq[Ranked[UKPRN]] = {
        val searchWord = word.trim().toLowerCase
        if (searchWord != "") {
          val wordMatches = keywordMap.keys.filter(_.startsWith(searchWord)).flatMap { key =>
            keywordMap(key).map(prn => Ranked(prn, 1.0 + searchWord.length.toDouble / key.length))
          }
          val codeMatches = codeMap.keys.filter(_ == searchWord).flatMap(codeMap(_).map(prn => Ranked(prn, 2.0)))
          (wordMatches ++ codeMatches).toSeq
        } else Seq()
      }
    }
  }

  def extractMaps = {
    val maps = dataStore.providers.values.map { provider =>
      val subjects = dataStore.apprenticeships.find(_.provider_id == provider.ukprn).map { a =>
        (a.subject_tier_2_code, a.subject_tier_2_title)
      }.toList.distinct

      val keywords = provider.name.split("\\s").map(_.trim.toLowerCase) ++ subjects.flatMap(_._2.split("\\s").map(_.trim.toLowerCase))
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