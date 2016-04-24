package apprenticeshipScorecard.controllers

object Filters {
  type Filter[T] = (Seq[T]) => Seq[T]

  def limit[T](l: Option[Int]): Filter[T] = { xs: Seq[T] =>
    l match {
      case Some(i) => xs.take(i)
      case _ => xs
    }
  }

  def sortBy[T, B](f: T => B)(implicit ordering: Ordering[B]): Filter[T] = { xs: Seq[T] => xs.sortBy(f)(ordering) }

  def limitAndSort[T, B](maxResults: Option[Int], sortF: T => B)(implicit ordering: Ordering[B]): Seq[Filter[T]] = Seq[Filter[T]](
    limit(maxResults),
    sortBy(sortF)
  )

  def filter[T](xs: Seq[T])(implicit filters: Seq[Filter[T]]): Seq[T] = filters.foldLeft(xs) { case (x, f) => f(x) }
}
