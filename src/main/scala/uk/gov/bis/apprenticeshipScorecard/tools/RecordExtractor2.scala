package uk.gov.bis.apprenticeshipScorecard.tools

import play.api.libs.json.{JsObject, JsString, JsValue}

import scala.collection.immutable.Iterable

object RecordExtractor2 {
  def extract(fields: Map[String, String]) : (JsObject, JsObject) = {
    val provider = extract(fields, FieldMappings.providerMappings)
    val apprenticeship = extract(fields, FieldMappings.apprenticeshipMappings)

    (provider, apprenticeship)
  }

  def extract(fields: Map[String, String], mappings: Map[Binding, String]): JsObject = {
    val fs: Iterable[Option[(String, JsValue)]] = mappings.map {
      case (SimpleBinding(fieldName), jsonFieldName) =>
        fields.get(fieldName).filter(nonBlank).map { value =>
          (jsonFieldName, JsString(value))
        }
      case (StructureBinding(ms), jsonFieldName) => Some((jsonFieldName, extract(fields, ms)))
    }

    JsObject(fs.toSeq.flatten)
  }

  def nonBlank(s: String): Boolean = s.trim != ""
}
