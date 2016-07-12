package uk.gov.bis.apprenticeshipScorecard.models

import play.api.libs.json._

case class JoinOne[T1, T2](primary: T1, secondary: T2, joinName: String)

object JoinOne {
  implicit def writes[T1, T2](implicit w1: Writes[T1], w2: Writes[T2]) = new Writes[JoinOne[T1, T2]] {
    override def writes(j: JoinOne[T1, T2]): JsValue = {
      val o1 = Json.toJson(j.primary).as[JsObject]
      val o2 = Json.toJson(j.secondary)

      o1 + (j.joinName -> o2)
    }
  }
}

case class JoinMany[T1, T2](primary: T1, secondary: Seq[T2], joinName: String)

object JoinMany {
  implicit def writes[T1, T2](implicit w1: Writes[T1], w2: Writes[T2]) = new Writes[JoinMany[T1, T2]] {
    override def writes(j: JoinMany[T1, T2]): JsValue = {
      val o1 = Json.toJson(j.primary).as[JsObject]
      val o2 = Json.toJson(j.secondary)

      o1 + (j.joinName -> o2)
    }
  }
}