package uk.gov.bis.apprenticeshipScorecard.controllers

import com.wellfactored.restless.play.actions.ApiActions._
import play.api.libs.json.{JsObject, Json, Writes}
import uk.gov.bis.apprenticeshipScorecard.tools.Haversine.Point
import uk.gov.bis.apprenticeshipScorecard.tools.{Haversine, Index, Ranked}

case class LocationSearchParams(point: Point, radius: Double)


trait Locatable[T] {
  def location(t: T): Option[Point]
}

trait SearchSupport[T] {

  implicit def locator: Locatable[T]

  implicit def writes: Writes[T]

  def index: Index[T]

  def withDistance(r: Ranked[T], point: Point, radius: Double): Option[Ranked[T]] = locator.location(r.item) match {
    case (Some(location)) =>
      val distance = Haversine.distance(point, location)
      if (distance <= radius) {
        // Somewhat arbitrary formula to calculate extra rank based on distance from search point
        val additionalRank = radius / (distance + 1) / radius * 3
        Some(r.addRank(additionalRank).withDistance(distance))
      } else None
    case _ => None
  }

  implicit class SearchSyntax(results: Seq[Ranked[T]]) {
    def searchLocation(ol: Option[LocationSearchParams]): Seq[Ranked[T]] =
      ol.map(searchLocation).getOrElse(results)

    def searchLocation(params: LocationSearchParams): Seq[Ranked[T]] =
      results.flatMap(withDistance(_, params.point, params.radius))
  }

  implicit class IndexSyntax(index: Index[T]) {
    def matchPhrase(op: Option[String]): Seq[Ranked[T]] = op match {
      case None => index.all
      case Some(phrase) => index.matchPhrase(phrase)
    }
  }

  def search(phrase: Option[String], lato: Option[Double], lono: Option[Double], disto: Option[Double]) = JsCollect {
    val lsp = for (lat <- lato; lon <- lono; dist <- disto)
      yield LocationSearchParams(Point(lat, lon), dist)

    index.matchPhrase(phrase).searchLocation(lsp).sortBy(_.rank).map(Json.toJson(_).as[JsObject])
  }
}
