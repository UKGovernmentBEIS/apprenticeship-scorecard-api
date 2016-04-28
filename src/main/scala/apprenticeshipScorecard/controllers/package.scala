package apprenticeshipScorecard

import com.wellfactored.restless.QueryAST.Query
import com.wellfactored.restless.play.json.JsonQuerying
import play.api.libs.json._


package object controllers {

  trait JsonExtractor

  implicit class Filtering[T: Writes](xs: Seq[T]) {
    def limit(l: Option[Int]): Seq[T] = l match {
      case Some(i) => xs.take(i)
      case None => xs
    }


    def extract(eo: Option[JsonExtractor]): Seq[JsObject] = eo match {
      case None => xs.map(x => Json.toJson(x).as[JsObject])
      case Some(e) => xs.map(x => Json.toJson(x).as[JsObject])
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

