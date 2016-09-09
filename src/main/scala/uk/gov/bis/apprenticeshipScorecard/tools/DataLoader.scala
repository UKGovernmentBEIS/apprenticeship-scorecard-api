package uk.gov.bis.apprenticeshipScorecard.tools

import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.json.{JsObject, Json}
import uk.gov.bis.apprenticeshipScorecard.models._

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

case class Subject(subject_tier_2_code: SubjectCode, subject_tier_2_title: String)

case class DataStore(
                      providers: Map[UKPRN, Provider],
                      apprenticeships: Seq[Apprenticeship],
                      subjects: Map[SubjectCode, Subject]
                    ) {

  import DataStore.{ApprenticeshipWithProvider, ProviderWithApprenticeships}

  lazy val apprenticeshipsJs = apprenticeshipsWithProvider.toSeq.sortBy(_.primary.description).map(Json.toJson(_).as[JsObject])
  lazy val providersJs = providers.values.toSeq.sortBy(_.ukprn.id).map(Json.toJson(_).as[JsObject])

  lazy val providersWithApprenticeships: Iterable[ProviderWithApprenticeships] = providers.values.map { provider =>
    Join(provider, apprenticeships.filter(_.provider_id == provider.ukprn), "apprenticeships")
  }

  lazy val apprenticeshipsWithProvider: Iterable[ApprenticeshipWithProvider] = apprenticeships.map { a =>
    Join(a, providers(a.provider_id), "provider")
  }
}

case class LineError(lineNumber: Int, error: String, fields: Map[String, String])

object DataStore {
  val empty = DataStore(Map(), Seq(), Map())

  type ApprenticeshipWithProvider = Apprenticeship Join Provider
  type ProviderWithApprenticeships = Provider Join Seq[Apprenticeship]
}

object DataLoader {

  lazy val dataStore: DataStore = Await.result(loadFromMongo, Duration(60, SECONDS))

  def loadFromMongo: Future[DataStore] = {
    for {
      providers <- MongoStore.readProviders
      apprenticeships <- MongoStore.readApprenticeships
    } yield {
      val subjects = apprenticeships.map(a => a.subject_tier_2_code -> Subject(a.subject_tier_2_code, a.description)).toMap
      val providerMap: Map[UKPRN, Provider] = providers.groupBy(_.ukprn).flatMap {
        case (k, v :: vs) => Some((k, v))
        case _ => None
      }
      DataStore(providerMap, apprenticeships, subjects)
    }
  }

}
