package uk.gov.bis.apprenticeshipScorecard.controllers

import javax.inject.Inject

import com.wellfactored.restless.play.actions.ApiActions._
import play.api.libs.json.{JsObject, Json}
import play.api.mvc.{Action, Controller}
import uk.gov.bis.apprenticeshipScorecard.models.{Apprenticeship, Provider, UKPRN}
import uk.gov.bis.apprenticeshipScorecard.tools.{ProviderIndex, Ranked, TSVLoader}

import scala.concurrent.ExecutionContext

class Providers @Inject()(implicit ec: ExecutionContext) extends Controller {

  implicit val providerFormat = Json.format[Provider]
  implicit val apprenticeshipFormat = Json.format[Apprenticeship]

  import TSVLoader.dataStore

  def find(ukprn: UKPRN) = Action { implicit request =>
    dataStore.providers.get(ukprn) match {
      case None => NotFound
      case Some(p) => Ok(Json.toJson(p))
    }
  }

  implicit class ProviderIndexSyntax(index: ProviderIndex) {
    def matchPhrase(op: Option[String]): Seq[Ranked[Provider]] = op match {
      case None => index.all
      case Some(phrase) => index.matchPhrase(phrase)
    }
  }

  implicit class SearchSyntax(results: Seq[Ranked[Provider]]) {
    def searchLocation(ol: Option[LocationSearchParams]): Seq[Ranked[Provider]] = ol match {
      case None => results
      case Some(params) => results.flatMap { rp =>
        (rp.item.address.latitude, rp.item.address.longitude) match {
          case (Some(lat), Some(lon)) =>
            val distance = haversineDistance(params.point, Point(lat.doubleValue(), lon.doubleValue()))
            if (distance <= params.radius) {
              // Somewhat arbitrary formula to calculate extra rank based on distance from search point
              val additionalRank = params.radius / (distance + 1) / params.radius * 3
              Some(rp.addRank(additionalRank))
            } else None
          case _ => None
        }
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

  def search(phrase: Option[String], lato: Option[Double], lono: Option[Double], disto: Option[Double]) = JsCollect {
    val lsp = for {
      lat <- lato
      lon <- lono
      dist <- disto
    } yield LocationSearchParams(Point(lat, lon), dist)

    ProviderIndex.matchPhrase(phrase).searchLocation(lsp).sortBy(_.rank).map(Json.toJson(_).as[JsObject])
  }

  def apprenticeships(ukprn: UKPRN) = JsCollect {
    dataStore.apprenticeships.filter(_.provider_id == ukprn).sortBy(_.description).map(Json.toJson(_).as[JsObject])
  }

  def providers = JsCollect(dataStore.providersJs)
}
