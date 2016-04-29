package apprenticeshipScorecard.controllers

import play.api.libs.json._

trait Projection[T1, T2] {
  def project(o: T1): T2
}

class JsonIdentity[T: Writes] extends Projection[T, JsObject] {
  override def project(o: T): JsObject = Json.toJson(o).as[JsObject]
}

class JsonProjector[T: Writes](paths: List[List[String]]) extends Projection[T, JsObject] {
  override def project(o: T): JsObject = {
    projectJs(Json.toJson(o))
  }

  def projectJs(value: JsValue): JsObject = {
    val parts = paths.flatMap { path =>
      val v = path.foldLeft(value) { case (j, p) => (j \ p).getOrElse(JsNull) }
      v match {
        case JsNull => None
        case jv => Some(path.reverse.foldLeft(jv) { case (j, p) => JsObject(Seq(p -> j)) }.as[JsObject])
      }
    }
    parts.foldLeft(JsObject(Seq())) { case (o1, o2) => o1.deepMerge(o2) }
  }
}