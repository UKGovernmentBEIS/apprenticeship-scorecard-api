package uk.gov.bis.apprenticeshipScorecard.models

import org.scalatest.{Matchers, WordSpecLike}
import play.api.libs.json.Json

class JoinOneTest extends WordSpecLike with Matchers {

  case class Foo(a: String, b: Int)

  object Foo {
    implicit val w = Json.writes[Foo]
  }

  case class Bar(x: String, y: Int)

  object Bar {
    implicit val w = Json.writes[Bar]
  }

  "JoinOne" should {
    "generate correct json" in {
      val j = JoinOne[Foo, Bar](Foo("foo", 1), Bar("bar", 2), "bar")

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

      JoinOne.writes[Foo, Bar].writes(j) shouldBe expected
    }
  }
}
