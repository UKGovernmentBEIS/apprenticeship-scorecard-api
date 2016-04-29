package apprenticeshipScorecard.controllers

import org.scalatest.{FlatSpec, Matchers}
import play.api.libs.json.{JsObject, Json}

class JsonProjectorTest extends FlatSpec with Matchers {

  "JsonProjector" should "extract a value from a Json object" in {
    val paths = List(List("a"))
    val json = Json.parse("""{ "a" : 5 }""").as[JsObject]

    val projector = new JsonProjector[JsObject](paths)
    projector.project(json) shouldBe json
  }

  it should "extract a nested value from a Json Object" in {
    val paths = List(List("a", "b"))
    val json = Json.parse("""{ "a" : { "b" : 5 } }""").as[JsObject]

    val projector = new JsonProjector[JsObject](paths)
    projector.project(json) shouldBe json
  }

  it should "extract one of two values from a Json Object" in {
    val paths = List(List("a"))
    val json = Json.parse("""{ "a" : 5, "b" : 4 }""").as[JsObject]
    val expected = Json.parse("""{ "a" : 5 }""").as[JsObject]

    val projector = new JsonProjector[JsObject](paths)
    projector.project(json) shouldBe expected
  }

  it should "extract both values from a Json Object" in {
    val paths = List(List("a"), List("b"))
    val json = Json.parse("""{ "a" : 5, "b" : 4 }""").as[JsObject]

    val projector = new JsonProjector[JsObject](paths)
    projector.project(json) shouldBe json
  }

  it should "extract a structure from a Json Object" in {
    val paths = List(List("x"))
    val json = Json.parse("""{ "x" : { "a" : 5, "b" : 4 } }""").as[JsObject]

    val projector = new JsonProjector[JsObject](paths)
    projector.project(json) shouldBe json
  }

  it should "extract and combine two parts of a structure from a Json Object" in {
    val paths = List(List("x", "a"), List("x", "b"))
    val json = Json.parse("""{ "x" : { "a" : 5, "b" : 4, "c" : 3 } }""").as[JsObject]
    val expected = Json.parse("""{ "x" : { "a" : 5, "b" : 4 } }""").as[JsObject]

    val projector = new JsonProjector[JsObject](paths)
    projector.project(json) shouldBe expected
  }

}
