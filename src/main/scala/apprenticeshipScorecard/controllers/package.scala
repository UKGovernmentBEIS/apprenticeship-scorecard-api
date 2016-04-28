package apprenticeshipScorecard

import apprenticeshipScorecard.queries.JsonQuerying
import atto.ParseResult.Done
import com.wellfactored.restless.QueryAST.Query
import com.wellfactored.restless.QueryParser
import play.api.libs.json.{JsError, JsPath, JsSuccess, _}


package object controllers {

  implicit class Filtering[T: Writes](xs: Seq[T]) {
    def limit(l: Option[Int]): Seq[T] = l match {
      case Some(i) => xs.take(i)
      case None => xs
    }

    def query(qo: Option[Query]): Seq[T] = qo match {
      case None => xs
      case Some(q) => xs.filter { x =>
        Json.toJson(x) match {
          case doc: JsObject => JsonQuerying.query(q)(doc)
          case _ => false
        }
      }
    }
  }


}

