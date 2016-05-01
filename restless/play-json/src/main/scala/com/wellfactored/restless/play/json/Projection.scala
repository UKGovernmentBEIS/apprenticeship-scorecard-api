package com.wellfactored.restless.play.json

import play.api.libs.json._

trait Projection[T1, T2] {
  def project(o: T1): T2
}

class JsonIdentity[T: Writes] extends Projection[T, JsObject] {
  override def project(o: T): JsObject = Json.toJson(o).as[JsObject]
}

/**
  * Given a list of paths (each of which is described by a list of String path components) this
  * will convert the T to a JsObject and extract from it only those parts that are referenced
  * by the paths.
  *
  * If a path references part of the json structure that does not exist then that path will
  * be silently ignored.
  *
  * @param paths references for parts of the json structure to be extracted
  * @tparam T the type of the object that the projection will operate on. There must be
  *           an implicit json.Writes instance for T in scope when the class is instantiated.
  */
class JsonProjector[T: Writes](paths: List[List[String]]) extends Projection[T, JsObject] {
  override def project(o: T): JsObject = projectJs(Json.toJson(o))

  def projectJs(value: JsValue): JsObject = {
    val parts = paths.flatMap { path =>
      // Use the components of this path to walk down the json structure
      val v = path.foldLeft(value) { case (j, p) => (j \ p).getOrElse(JsNull) }
      v match {
        case JsNull => None
        // if the path has given us a json value then create a new json object structure
        // of the same shape as the original containing the extracted value. By reversing
        // the list of path elements we build the structure from the innermost part out.
        case jv => Some(path.reverse.foldLeft(jv) { case (j, p) => JsObject(Seq(p -> j)) }.as[JsObject])
      }
    }

    // Merge all of the resulting json objects back together to form our result
    parts.foldLeft(JsObject(Seq())) { case (o1, o2) => o1.deepMerge(o2) }
  }
}