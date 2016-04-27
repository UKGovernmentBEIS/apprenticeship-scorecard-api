package apprenticeshipScorecard.queries

import org.scalatest.{FlatSpec, Matchers}
import play.api.libs.json._

class JsonQuerying$Test extends FlatSpec with Matchers{
  import JsonQuerying._

  import atto._
  import Atto._

  "query" should "return true for equal string with a single-level path" in {
    val json = Json.parse("""{ "a" : "foo" }""")
    val q = QueryParser.query.parseOnly("""a = "foo"""").option.get

    query(json, q) shouldBe true
  }

  it should "return true for equal string with a two-level path" in {
    val json = Json.parse("""{ "a" : { "b" :"foo"} }""")
    val q = QueryParser.query.parseOnly("""a.b = "foo"""").option.get

    query(json, q) shouldBe true
  }

  it should "return true for a conjunction test" in {
    val json = Json.parse("""{ "a" : { "b" :"foo", "c" : 3.0} }""")
    val q = QueryParser.query.parseOnly("""a.b = "foo" and a.c = 3""").option.get

    query(json, q) shouldBe true
  }

}
