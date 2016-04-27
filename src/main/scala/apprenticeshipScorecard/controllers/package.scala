package apprenticeshipScorecard

import apprenticeshipScorecard.queries.JsonQuerying
import apprenticeshipScorecard.queries.QueryAST.Query
import play.api.libs.json.{Json, Writes}


package object controllers {

  implicit class Filtering[T: Writes](xs: Seq[T]) {
    def limit(l: Option[Int]): Seq[T] = l match {
      case Some(i) => xs.take(i)
      case None => xs
    }

    def query(qo: Option[Query]): Seq[T] = qo match {
      case None => xs
      case Some(q) => xs.filter { x => JsonQuerying.query(Json.toJson(x), q) }
    }
  }
}

