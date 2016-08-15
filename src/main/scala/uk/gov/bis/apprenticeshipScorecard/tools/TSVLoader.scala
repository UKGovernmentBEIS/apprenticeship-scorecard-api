package uk.gov.bis.apprenticeshipScorecard.tools

import uk.gov.bis.apprenticeshipScorecard.models._
import cats.data.Validated
import cats.data.Validated.{Invalid, Valid}
import cats.std.list._
import play.api.libs.json.{JsObject, Json}

import scala.io.Source

case class Subject(subject_tier_2_code: SubjectCode, subject_tier_2_title: String)

case class DataStore(
                      columnNames: Seq[String],
                      providers: Map[UKPRN, Provider],
                      apprenticeships: Seq[Apprenticeship],
                      subjects: Map[SubjectCode, Subject],
                      errors: Seq[LineError]
                    ) {

  import DataStore.{ProviderWithApprenticeships, ApprenticeshipWithProvider}

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
  val empty = DataStore(Seq(), Map(), Seq(), Map(), Seq())

  type ApprenticeshipWithProvider = Apprenticeship Join Provider
  type ProviderWithApprenticeships = Provider Join Seq[Apprenticeship]
}

object TSVLoader {

  val fileName = "data/Scorecard_Data_v5.tsv"

  lazy val dataStore: DataStore = loadFromSource(Source.fromFile(fileName))

  def main(args: Array[String]): Unit = {
    val data = dataStore

    println(s"Successfully loaded ${data.providers.keys.size} providers and ${data.apprenticeships.length} apprenticeships")

    println(data.apprenticeships.count(_.learner_stats.satisfaction.isDefined) + " apprenticeships with learner stats")

    data.errors.foreach { case LineError(line, e, fields) => println(s"line $line: $e"); println(fields) }
    if (data.errors.nonEmpty) println(s"total errors: ${data.errors.length}")
    println(data.columnNames)
  }

  def loadFromSource(source: Source): DataStore = {
    val lines = source.getLines.toList
    val ds = lines.headOption.map(_.split("\t").toList) match {
      case None => DataStore.empty
      case Some(colNames) => processData(lines, colNames)
    }

    source.close()

    ds
  }

  def processData(lines: List[String], colNames: List[String]): DataStore = {
    val results = parseRecords(lines, colNames)

    val (providers, apprenticeships) = results.collect { case Valid(p) => p }.unzip

    val providerMap = providers.groupBy(_.ukprn).flatMap {
      case (k, v :: vs) => Some((k, v))
      case _ => None
    }

    val subjects = apprenticeships.map(a => a.subject_tier_2_code -> Subject(a.subject_tier_2_code, a.subject_tier_2_title)).toMap

    val errs = results.collect { case Invalid(es) => es }.flatten

    DataStore(colNames, providerMap, apprenticeships, subjects, errs)
  }


  def parseRecords(lines: List[String], colNames: List[String]): List[Validated[List[LineError], (Provider, Apprenticeship)]] = {
    lines.tail.zipWithIndex.map {
      case (record, idx) =>
        val fieldValues = record.split("\t").toList
        val fields = colNames.zip(fieldValues).toMap

        RecordExtractor.extract(fields) match {
          case v@Valid(_) => v
          case Invalid(es) => Invalid(es.unwrap.map(LineError(idx, _, fields)))
        }
    }
  }
}
