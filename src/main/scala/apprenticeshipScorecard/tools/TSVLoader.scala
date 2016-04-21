package apprenticeshipScorecard.tools

import apprenticeshipScorecard.models.{Apprenticeship, Provider, UKPRN}
import cats.data.Validated
import cats.data.Validated.{Invalid, Valid}
import cats.std.list._

import scala.io.Source

case class DataStore(
                      providers: Map[UKPRN, Provider],
                      apprenticeships: List[Apprenticeship],
                      errors: List[(Int, String)]
                    )

object TSVLoader {

  def main(args: Array[String]): Unit = {
    val fileName = "data/Scorecard_Analysis_Data_For_Site_v4_fictional.tsv"

    val data = loadFromSource(Source.fromFile(fileName))

    println(s"Successfully loaded ${data.providers.keys.size} providers and ${data.apprenticeships.length} apprenticeships")

    data.errors.foreach { case (line, e) => println(s"line $line: $e") }
    if (data.errors.nonEmpty) println(s"total errors: ${data.errors.length}")

  }

  def loadFromSource(source: Source): DataStore = {
    val lines = source.getLines.toList
    val colNames = lines.head.split("\t").toList

    processData(lines, colNames)
  }

  def processData(lines: List[String], colNames: List[String]): DataStore = {
    val results = parseRecords(lines, colNames)

    val (providers, apprenticeships) = results.collect { case Valid(p) => p }.unzip

    val providerMap = providers.groupBy(_.ukprn).map { case (k, vs) => (k, vs.head) }

    val errs = results.collect { case Invalid(es) => es }.flatten

    DataStore(providerMap, apprenticeships, errs)
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