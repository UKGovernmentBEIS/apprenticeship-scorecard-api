package uk.gov.bis.apprenticeshipScorecard.tools

object Haversine {

  case class Point(lat: Double, lon: Double)

  /**
    * Adapted from https://davidkeen.com/blog/2013/10/calculating-distance-with-scalas-foldleft
    * Default earth radius is in miles
    */
  def distance(pointA: Point, pointB: Point, earthRadius: Double = 3958.761): Double = {
    val deltaLat = math.toRadians(pointB.lat - pointA.lat)
    val deltaLong = math.toRadians(pointB.lon - pointA.lon)
    val a = math.pow(math.sin(deltaLat / 2), 2) +
      math.cos(math.toRadians(pointA.lat)) *
        math.cos(math.toRadians(pointB.lat)) *
        math.pow(math.sin(deltaLong / 2), 2)
    val greatCircleDistance = 2 * math.atan2(math.sqrt(a), math.sqrt(1 - a))
    earthRadius * greatCircleDistance
  }
}
