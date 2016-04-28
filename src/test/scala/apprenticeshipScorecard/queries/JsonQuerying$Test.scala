package apprenticeshipScorecard.queries

import org.scalatest.{FlatSpec, Matchers}
import play.api.libs.json._

class JsonQuerying$Test extends FlatSpec with Matchers {

  import JsonQuerying._

  import atto._
  import Atto._

  "query" should "return true for equal string with a single-level path" in {
    implicit val doc = Json.parse("""{ "a" : "foo" }""").as[JsObject]
    val q = QueryParser.query.parseOnly("""a = "foo"""").option.get

    query(q) shouldBe true
  }

  it should "return true for equal string with a two-level path" in {
    implicit val doc = Json.parse("""{ "a" : { "b" :"foo"} }""").as[JsObject]
    val q = QueryParser.query.parseOnly("""a.b = "foo"""").option.get

    query(q) shouldBe true
  }

  it should "return true for a comparison against another path" in {
    implicit val doc = Json.parse("""{ "a" : 3.0, "b" : 3.0 }""").as[JsObject]
    val q = QueryParser.query.parseOnly("""a = b""").option.get

    query(q) shouldBe true
  }

  it should "return true for a conjunction test" in {
    implicit val doc = Json.parse("""{ "a" : { "b" :"foo", "c" : 3.0} }""").as[JsObject]
    val q = QueryParser.query.parseOnly("""a.b = "foo" and a.c = 3""").option.get

    query(q) shouldBe true
  }

}
