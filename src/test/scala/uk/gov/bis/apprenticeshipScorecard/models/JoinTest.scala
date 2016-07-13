package uk.gov.bis.apprenticeshipScorecard.models

import org.scalatest.{Matchers, WordSpecLike}
import play.api.libs.json.Json

class JoinTest extends WordSpecLike with Matchers {

  case class Foo(a: String, b: Int)

  object Foo {
    implicit val w = Json.writes[Foo]
  }

  case class Bar(x: String, y: Int)

  object Bar {
    implicit val w = Json.writes[Bar]
  }

  "JoinOne" should {
    "generate correct json for a one-to-one" in {
      val j = Join[Foo, Bar](Foo("foo", 1), Bar("bar", 2), "bar")

      val expected = Json.parse(
        """
          |{
          |  "a" : "foo",
          |  "b" : 1,
          |  "bar" : {
          |      "x" : "bar",
          |      "y" : 2
          |  }
          |}
        """.stripMargin
      )

      Join.writes[Foo, Bar].writes(j) shouldBe expected
    }

    "generate correct json for one-to-many" in {
      val j = Join[Foo, Seq[Bar]](Foo("foo", 1), Seq(Bar("bar1", 1), Bar("bar2", 2)), "bars")

      val expected = Json.parse(
        """
          |{
          |  "a" : "foo",
          |  "b" : 1,
          |  "bars" : [
          |      { "x" : "bar1", "y" : 1 },
          |      { "x" : "bar2", "y" : 2 }
          |  ]
          |}
        """.stripMargin
      )

      Join.writes[Foo, Seq[Bar]].writes(j) shouldBe expected
    }
  }
}
