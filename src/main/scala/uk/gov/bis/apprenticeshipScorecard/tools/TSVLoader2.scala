package uk.gov.bis.apprenticeshipScorecard.tools

import cats.data.Validated
import cats.data.Validated.{Invalid, Valid}
import play.api.libs.json.{JsObject, Json}
import uk.gov.bis.apprenticeshipScorecard.models._

import scala.io.Source



object TSVLoader2 {

  val fileName = "data/Scorecard_Data_v5.tsv"

  lazy val dataStore: DataStore = loadFromSource(Source.fromFile(fileName))

  def main(args: Array[String]): Unit = {
    val data = dataStore

    println(s"Successfully loaded ${data.providers.keys.size} providers and ${data.apprenticeships.length} apprenticeships")

    println(data.apprenticeships.count(_.learner_stats.satisfaction.isDefined) + " apprenticeships with learner stats")

    data.errors.foreach { case LineError(line, e, fields) => println(s"line $line: $e"); println(fields) }
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

    val providerMap = providers.groupBy(_.ukprn).flatMap {
      case (k, v :: vs) => Some((k, v))
      case _ => None
    }

    val subjects = apprenticeships.map(a => a.subject_tier_2_code -> Subject(a.subject_tier_2_code, a.subject_tier_2_title)).toMap

    val errs = results.collect { case Invalid(es) => es }.flatten

    DataStore(colNames, providerMap, apprenticeships, subjects, errs)
  }


  def parseRecords(lines: List[String], colNames: List[String]): List[Validated[List[LineError], (Provider, Apprenticeship)]] = {
    val os = lines.tail.zipWithIndex.map {
      case (record, idx) =>
        val fieldValues = record.split("\t").toList
        val fields = colNames.zip(fieldValues).toMap

        RecordExtractor2.extract(fields)
    }

    os.foreach(println)



    ???
  }
}
