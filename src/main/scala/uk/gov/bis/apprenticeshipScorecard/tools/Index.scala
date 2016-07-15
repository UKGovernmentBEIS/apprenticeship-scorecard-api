package uk.gov.bis.apprenticeshipScorecard.tools

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

  def normalise(s: String): String = s.trim.toLowerCase

  def splitWords(s: String): List[String] = s.split("\\s").toList

  def mergeMaps[A](maps: Iterable[Map[String, A]]): Map[String, Set[A]] = {
    maps.foldLeft(Map[String, Set[A]]()) { case (alreadyMerged, next) =>
      alreadyMerged ++ next.map { case (k, v) => k -> alreadyMerged.get(k).map(_ + v).getOrElse(Set(v)) }
    }
  }

  def lookupWord(word: String, prefixMap: Map[String, Set[T]], fullwordMap: Map[String, Set[T]]): Seq[Ranked[T]] = {
    val searchWord = word.trim().toLowerCase
    if (searchWord != "") {
      val wordMatches = prefixMap.keys.filter(_.startsWith(searchWord)).flatMap { key =>
        prefixMap(key).map(prn => Ranked(prn, 1.0 + searchWord.length.toDouble / key.length))
      }
      val codeMatches = fullwordMap.keys.filter(_ == searchWord).flatMap(fullwordMap(_).map(prn => Ranked(prn, 2.0)))
      (wordMatches ++ codeMatches).toSeq
    } else Seq()
  }

}

case class Ranked[T](item: T, rank: Double, distance: Option[Double] = None) {
  def addRank(r: Double): Ranked[T] = copy(rank = this.rank + r)

  def withDistance(d: Double): Ranked[T] = copy(distance = Some(d))
}