package apprenticeshipScorecard.controllers

case class PageNumber(num: Int) extends AnyVal

case class PageCount(count: Int) extends AnyVal {
  /*
  * Limit the supplied page number to be no bigger than this page count
   */
  def limit(pageNumber: PageNumber): PageNumber = PageNumber(count.min(pageNumber.num))
}

case class ResultsPage[T](resultCount: Int, maxResults: Int, pageCount: PageCount, selectedPage: PageNumber, perPage: PageCount, resultsForPage: Seq[T]) {
  val tooManyResults = resultCount > maxResults
  val currentPage: PageNumber = pageCount.limit(selectedPage)
  val hasResults = resultsForPage.nonEmpty
}

object ResultsPage {
  def build[T](results: Seq[T], selectedPage: PageNumber, maxResults: Int = 100, perPage: PageCount = PageCount(10)): ResultsPage[T] = {
    require(maxResults > 0)
    require(perPage.count > 0)
    assume(maxResults > 0)

    val cappedResults = results.take(maxResults)

    val resultCount = results.length
    val pageCount = PageCount((cappedResults.length / perPage.count) + (if (cappedResults.length % perPage.count > 0) 1 else 0))
    val currentPage = pageCount.limit(selectedPage)

    val hasResults = results.nonEmpty
    val tooManyResults = results.length > maxResults

    //noinspection DropTakeToSlice
    val resultsForPage: Seq[T] = cappedResults.drop((currentPage.num - 1) * perPage.count).take(perPage.count)

    ResultsPage(resultCount, maxResults, pageCount, currentPage, perPage, resultsForPage)
  }


}

