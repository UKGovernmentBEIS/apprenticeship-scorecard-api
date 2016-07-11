package uk.gov.bis.apprenticeshipScorecard.tools

import uk.gov.bis.apprenticeshipScorecard.models.{Provider, SubjectCode, UKPRN}

trait Index[T] {
  /**
    * Match a single word against the index
    *
    * @param word the word to match
    * @return a set of entries with a matching rank
    */
  def lookup(word: String): Seq[Ranked[T]]

  def all: Seq[Ranked[T]]

  /**
    * Find entries that (at least partially) match *all* the words in the
    * phrase
    */
  def matchPhrase(phrase: String): Seq[Ranked[T]] = {
    val resultSets = phrase.trim.split("\\s").toList.map(lookup)

    // find items that are in all the result sets
    val matches = resultSets.foldLeft(resultSets.flatten) { case (remainingResults, resultSet) =>
      remainingResults.filter(r => resultSet.exists(_.item == r.item))
    }

    for {
      (k, vs) <- matches.groupBy(_.item).toSeq
    } yield Ranked(k, vs.map(_.rank).sum)
  }
}

case class Ranked[T](item: T, rank: Double, distance: Option[Double] = None) {
  def addRank(r: Double): Ranked[T] = copy(rank = this.rank + r)

  def withDistance(d: Double): Ranked[T] = copy(distance = Some(d))
}

object ProviderIndex extends ProviderIndex(TSVLoader.dataStore)

class ProviderIndex(dataStore: DataStore) extends Index[Provider] {
  override def lookup(s: String): Seq[Ranked[Provider]] = for {
    rankedPrn <- index.lookup(s)
    provider <- dataStore.providers.get(rankedPrn.item)
  } yield Ranked(provider, rankedPrn.rank)

  override def all: Seq[Ranked[Provider]] = dataStore.providers.values.map(Ranked(_, 1)).toSeq

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

      override def all: Seq[Ranked[UKPRN]] = dataStore.providers.keys.map(Ranked(_, 1)).toSeq
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
  def extractWordIndices: (Iterable[Map[String, UKPRN]], Iterable[Map[String, UKPRN]]) =
    dataStore.providers.values.map { provider =>
      val (subjectCodes, subjectTitles) = extractSubjects(provider)

      val keywords = (splitWords(provider.name) ++ subjectTitles.flatMap(splitWords)).map(normalise)

      (Map(keywords.map(k => k -> provider.ukprn): _*), Map(subjectCodes.map(sc => sc.code.toString -> provider.ukprn): _*))
    }.unzip


  def extractSubjects(provider: Provider): (List[SubjectCode], List[String]) = {
    provider.apprenticeships.map { a =>
      (a.subject_tier_2_code, a.subject_tier_2_title)
    }.toList.distinct.unzip
  }

  def mergeMaps(maps: Iterable[Map[String, UKPRN]]): Map[String, Set[UKPRN]] = {
    maps.foldLeft(Map[String, Set[UKPRN]]()) { case (alreadyMerged, stringToUkprn) =>
      alreadyMerged ++ stringToUkprn.map { case (k, v) => k -> alreadyMerged.get(k).map(_ + v).getOrElse(Set(v)) }
    }
  }
}