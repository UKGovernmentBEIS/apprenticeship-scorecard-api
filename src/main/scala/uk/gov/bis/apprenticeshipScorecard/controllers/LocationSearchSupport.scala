package uk.gov.bis.apprenticeshipScorecard.controllers

import uk.gov.bis.apprenticeshipScorecard.tools.DataStore.{ApprenticeshipWithProvider, ProviderWithApprenticeships}
import uk.gov.bis.apprenticeshipScorecard.tools.Ranked

trait LocationSearchSupport {
  def location[T](r: Ranked[T], lato: Option[BigDecimal], lono: Option[BigDecimal], point: Point, radius: Double): Option[Ranked[T]] =
    (lato, lono) match {
      case (Some(lat), Some(lon)) =>
        val distance = haversineDistance(point, Point(lat.doubleValue(), lon.doubleValue()))
        if (distance <= radius) {
          // Somewhat arbitrary formula to calculate extra rank based on distance from search point
          val additionalRank = radius / (distance + 1) / radius * 3
          Some(r.addRank(additionalRank).withDistance(distance))
        } else None
      case _ => None
    }

  implicit class SearchSyntax(results: Seq[Ranked[ProviderWithApprenticeships]]) {
    def searchLocation(ol: Option[LocationSearchParams]): Seq[Ranked[ProviderWithApprenticeships]] = ol match {
      case None => results
      case Some(params) => searchLocation(params)
    }

    def searchLocation(params: LocationSearchParams): Seq[Ranked[ProviderWithApprenticeships]] = {
      results.flatMap { rp =>
        location(rp, rp.item.primary.address.latitude, rp.item.primary.address.longitude, params.point, params.radius)
      }
    }
  }

  implicit class SearchSyntax2(results: Seq[Ranked[ApprenticeshipWithProvider]]) {
    def searchLocation(ol: Option[LocationSearchParams]): Seq[Ranked[ApprenticeshipWithProvider]] = ol match {
      case None => results
      case Some(params) => searchLocation(params)
    }

    def searchLocation(params: LocationSearchParams): Seq[Ranked[ApprenticeshipWithProvider]] = {
      results.flatMap { rp =>
        location(rp, rp.item.secondary.address.latitude, rp.item.secondary.address.longitude, params.point, params.radius)
      }
    }
  }

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
