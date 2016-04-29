package apprenticeshipScorecard

import com.wellfactored.restless.QueryAST.Query
import com.wellfactored.restless.play.json.JsonQuerying
import play.api.libs.json._


package object controllers {

  trait Projection[T1, T2] {
    def project(o: T1): T2
  }

  class IdProjection[T] extends Projection[T, T] {
    override def project(o: T): T = o
  }

  implicit class Filtering[T: Writes](xs: Seq[T]) {
    def limit(l: Option[Int]): Seq[T] = l match {
      case Some(i) => xs.take(i)
      case None => xs
    }


    def project[T2](projection: Projection[T, T2]): Seq[T2] = xs.map(x => projection.project(x))

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

