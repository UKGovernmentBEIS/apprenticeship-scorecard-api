package uk.gov.bis.apprenticeshipScorecard.controllers

import com.wellfactored.restless.play.actions.ApiActions._
import play.api.libs.json.{JsObject, Json, Writes}
import uk.gov.bis.apprenticeshipScorecard.tools.{Index, Ranked}

case class LocationSearchParams(point: Point, radius: Double)

case class Point(lat: Double, lon: Double)

trait Locatable[T] {
  def location(t: T): Option[Point]
}

trait SearchSupport[T] {

  implicit def locator: Locatable[T]

  implicit def writes: Writes[T]

  def index: Index[T]

  implicit class SearchSyntax(results: Seq[Ranked[T]]) {
    def withDistance(r: Ranked[T], point: Point, radius: Double): Option[Ranked[T]] =
      locator.location(r.item) match {
        case (Some(location)) =>
          val distance = haversineDistance(point, location)
          if (distance <= radius) {
            // Somewhat arbitrary formula to calculate extra rank based on distance from search point
            val additionalRank = radius / (distance + 1) / radius * 3
            Some(r.addRank(additionalRank).withDistance(distance))
          } else None
        case _ => None
      }

    def searchLocation(params: LocationSearchParams): Seq[Ranked[T]] = {
      results.flatMap { rp =>
        withDistance(rp, params.point, params.radius)
      }
    }

    def searchLocation(ol: Option[LocationSearchParams]): Seq[Ranked[T]] = ol match {
      case None => results
      case Some(params) => searchLocation(params)
    }
  }

  /**
    * Adapted from https://davidkeen.com/blog/2013/10/calculating-distance-with-scalas-foldleft
    * Default earth radius is in miles
    */
  def haversineDistance(pointA: Point, pointB: Point, earthRadius: Double = 3958.761): Double = {
    val deltaLat = math.toRadians(pointB.lat - pointA.lat)
    val deltaLong = math.toRadians(pointB.lon - pointA.lon)
    val a = math.pow(math.sin(deltaLat / 2), 2) +
      math.cos(math.toRadians(pointA.lat)) *
        math.cos(math.toRadians(pointB.lat)) *
        math.pow(math.sin(deltaLong / 2), 2)
    val greatCircleDistance = 2 * math.atan2(math.sqrt(a), math.sqrt(1 - a))
    earthRadius * greatCircleDistance
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
