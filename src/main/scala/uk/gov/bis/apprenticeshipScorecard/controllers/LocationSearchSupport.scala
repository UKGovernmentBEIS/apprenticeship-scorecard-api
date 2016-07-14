package uk.gov.bis.apprenticeshipScorecard.controllers

import uk.gov.bis.apprenticeshipScorecard.tools.Ranked

trait Locatable[T] {
  def lat(t: T): Option[BigDecimal]

  def lon(t: T): Option[BigDecimal]
}

trait LocationSearchSupport {
  def withDistance[T](r: Ranked[T], point: Point, radius: Double)(implicit loc: Locatable[T]): Option[Ranked[T]] =
    (loc.lat(r.item), loc.lon(r.item)) match {
      case (Some(lat), Some(lon)) =>
        val distance = haversineDistance(point, Point(lat.doubleValue(), lon.doubleValue()))
        if (distance <= radius) {
          // Somewhat arbitrary formula to calculate extra rank based on distance from search point
          val additionalRank = radius / (distance + 1) / radius * 3
          Some(r.addRank(additionalRank).withDistance(distance))
        } else None
      case _ => None
    }

  trait SearchLocation[T] {
    def results: Seq[Ranked[T]]

    def searchLocation(ol: Option[LocationSearchParams])(implicit locator: Locatable[T]): Seq[Ranked[T]] = ol match {
      case None => results
      case Some(params) => searchLocation(params)
    }

    def searchLocation(params: LocationSearchParams)(implicit locator: Locatable[T]): Seq[Ranked[T]] = {
      results.flatMap { rp =>
        withDistance(rp, params.point, params.radius)
      }
    }
  }

  implicit class SearchSyntax[T: Locatable](override val results: Seq[Ranked[T]]) extends SearchLocation[T]

  case class Point(lat: Double, lon: Double)

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

  case class LocationSearchParams(point: Point, radius: Double)

}
