package uk.gov.bis.apprenticeshipScorecard.tools

import org.scalatest.{FlatSpec, Matchers, OptionValues}
import uk.gov.bis.apprenticeshipScorecard.models._

class IndexSpec extends FlatSpec with Matchers with OptionValues {
  val qs = QualificationStats(None, None, None)
  val ls = LearnerStats(None, None, None, None, None, None, None, None, None)
  val sc1 = SubjectCode(15.1)
  val p1 = Provider(UKPRN(1), "", "", "WOODSPEEN", "", "", "", "", Address(None, None, None, None, None, None, None), None)
  val p2 = Provider(UKPRN(2), "", "", "ACCESS TRAINING (EAST MIDLANDS) LTD", "", "", "", "", Address(None, None, None, None, None, None, None), None)
  val p3 = Provider(UKPRN(3), "", "", "ACCESS TO MUSIC", "", "", "", "", Address(None, None, None, None, None, None, None), None)
  val a1 = Apprenticeship(sc1, "", ls, "ACCOUNTANCY TRAINING", UKPRN(3), qs, qs, Earnings(None, None), Earnings(None, None), 0)

  val ds: DataStore = DataStore(
    Map(p1.ukprn -> p1, p2.ukprn -> p2, p3.ukprn -> p3),
    Seq(a1),
    Map(sc1 -> Subject(sc1, "")))

  val sut = new ProviderIndex(ds)

  "mergeMaps" should "correctly merge a single map" in {
    sut.mergeMaps(Seq(Map("foo" -> UKPRN(1)))) shouldBe Map("foo" -> Set(UKPRN(1)))
  }

  it should "correctly merge two maps" in {
    val map1 = Map("foo" -> UKPRN(1))
    val map2 = Map("foo" -> UKPRN(2))
    sut.mergeMaps(Seq(map1, map2)) shouldBe Map("foo" -> Set(UKPRN(1), UKPRN(2)))
  }

  it should "correctly merge three maps" in {
    val map1 = Map("foo" -> UKPRN(1))
    val map2 = Map("foo" -> UKPRN(2))
    val map3 = Map("bar" -> UKPRN(1))
    sut.mergeMaps(Seq(map1, map2, map3)) shouldBe Map(
      "foo" -> Set(UKPRN(1), UKPRN(2)),
      "bar" -> Set(UKPRN(1))
    )
  }

  "sut" should "match a subject code" in {
    sut.lookup("15.1").size shouldBe 1
  }

  it should "match a provider name" in {
    sut.lookup("WOODSPEEN").size shouldBe 1
  }

  it should "match the start of a provider name" in {
    val result = sut.lookup("WOOD")
    result.size shouldBe 1
    result.headOption.value.rank - 1.4444 should be < 0.1
  }

  it should "match two provider names" in {
    val results = sut.matchPhrase("ACCESS TRAINING")
    results.size shouldBe 2

    results.find(_.item.primary.name == "ACCESS TRAINING (EAST MIDLANDS) LTD").map(_.rank).value shouldBe 4.0
  }

}
