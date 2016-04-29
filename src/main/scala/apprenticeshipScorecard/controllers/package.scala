package apprenticeshipScorecard

import com.wellfactored.restless.QueryAST.Query
import com.wellfactored.restless.play.json.JsonQuerying
import play.api.libs.json._


package object controllers {


  implicit class Filtering[T: Writes](xs: Seq[T]) {
    def limit(l: Option[Int]): Seq[T] = l match {
      case Some(i) => xs.take(i)
      case None => xs
    }

    def where(qo: Option[Query]): Seq[T] = qo match {
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

