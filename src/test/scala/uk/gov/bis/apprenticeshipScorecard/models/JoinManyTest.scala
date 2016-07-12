package uk.gov.bis.apprenticeshipScorecard.models

import org.scalatest.{Matchers, WordSpecLike}
import play.api.libs.json.Json

class JoinManyTest extends WordSpecLike with Matchers {

  case class Foo(a: String, b: Int)

  object Foo {
    implicit val w = Json.writes[Foo]
  }

  case class Bar(x: String, y: Int)

  object Bar {
    implicit val w = Json.writes[Bar]
  }

  "JoinMany" should {
    "generate correct json" in {
      val j = JoinMany[Foo, Bar](Foo("foo", 1), Seq(Bar("bar1", 1), Bar("bar2", 2)), "bars")

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

      JoinMany.writes[Foo, Bar].writes(j) shouldBe expected
    }
  }
}
