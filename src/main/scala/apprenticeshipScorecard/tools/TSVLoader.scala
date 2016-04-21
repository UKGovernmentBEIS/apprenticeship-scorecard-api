package apprenticeshipScorecard.tools

import cats.data.Validated.{Invalid, Valid}
import cats.std.list._

object TSVLoader {
  def main(args: Array[String]): Unit = {
    val fileName = "data/Scorecard_Analysis_Data_For_Site_v4_fictional.tsv"

    import scala.io._

    val s: BufferedSource = Source.fromFile(fileName)

    val lines = s.getLines.toList

    val colNames = lines.head.split("\t").toList

    val results = lines.tail.zipWithIndex.map {
      case (record, idx) =>
        val fieldValues = record.split("\t").toList
        val fields = colNames.zip(fieldValues).toMap

        ProviderExtractor.extract(fields) match {
          case Valid(provider) => Valid(provider)
          case Invalid(es) => Invalid(es.unwrap.map(idx -> _))
        }
    }

    val errs = results.collect {
      case Invalid(es) => es
    }.flatten

    val providers = results.collect {
      case Valid(p) => p
    }

    errs.foreach { case (line, e) => println(s"line $line: $e") }
    if (errs.nonEmpty) println(s"total errors: ${errs.length}")

    println(s"Successfully loaded ${providers.length} providers")
  }
}
