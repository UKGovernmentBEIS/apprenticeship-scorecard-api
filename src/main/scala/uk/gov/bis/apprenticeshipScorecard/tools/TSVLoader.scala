package uk.gov.bis.apprenticeshipScorecard.tools

import uk.gov.bis.apprenticeshipScorecard.models.{Apprenticeship, Provider, SubjectCode, UKPRN}
import cats.data.Validated
import cats.data.Validated.{Invalid, Valid}
import cats.std.list._
import play.api.libs.json.{JsObject, Json}

import scala.io.Source

case class Subject(subject_tier_2_code: SubjectCode, subject_tier_2_title: String)

case class DataStore(
                      providers: Map[UKPRN, Provider],
                      apprenticeships: Seq[Apprenticeship],
                      subjects: Map[SubjectCode, Subject],
                      errors: Seq[(Int, String)]
                    ) {
  lazy val apprenticeshipsJs = apprenticeships.sortBy(_.description).map(Json.toJson(_).as[JsObject])
  lazy val providersJs = providers.values.toSeq.sortBy(_.ukprn.id).map(Json.toJson(_).as[JsObject])
}

object DataStore {
  val empty = DataStore(Map(), Seq(), Map(), Seq())
}

object TSVLoader {

  val fileName = "data/Scorecard_Analysis_Data_For_Site_v4_fictional.tsv"

  lazy val dataStore: DataStore = loadFromSource(Source.fromFile(fileName))


  def main(args: Array[String]): Unit = {
    val data = dataStore

    println(s"Successfully loaded ${data.providers.keys.size} providers and ${data.apprenticeships.length} apprenticeships")

    println(data.apprenticeships.count(_.learner_stats.satisfaction.isDefined) + " apprenticeships with learner stats")

    data.errors.foreach { case (line, e) => println(s"line $line: $e") }
    if (data.errors.nonEmpty) println(s"total errors: ${data.errors.length}")
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

    val providerMap = providers.groupBy(_.ukprn).map { case (k, vs) => (k, vs.head) }

    val subjects = apprenticeships.map(a => a.subject_tier_2_code -> Subject(a.subject_tier_2_code, a.subject_tier_2_title)).toMap


    val errs = results.collect { case Invalid(es) => es }.flatten

    DataStore(providerMap, apprenticeships, subjects, errs)
  }

  def parseRecords(lines: List[String], colNames: List[String]): List[Validated[List[(Int, String)], (Provider, Apprenticeship)]] = {
    lines.tail.zipWithIndex.map {
      case (record, idx) =>
        val fieldValues = record.split("\t").toList
        val fields = colNames.zip(fieldValues).toMap

        RecordExtractor.extract(fields) match {
          case v@Valid(_) => v
          case Invalid(es) => Invalid(es.unwrap.map(idx -> _))
        }
    }
  }
}
