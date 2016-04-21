package apprenticeshipScorecard.tools

import apprenticeshipScorecard.models.UKPRN
import cats.data.Validated.{Invalid, Valid}
import org.scalatest.{FlatSpec, Matchers}

class UKPRNExtractorTest extends FlatSpec with Matchers {

  import UKPRNExtractor._

  "loader" should "successfully extract" in {
    extract(Map(fieldName -> "123456 ")) shouldBe Valid(UKPRN(123456))
  }

  it should "fail to extract a non-numeric string" in {
    extract(Map(fieldName -> "not a number")) shouldBe an[Invalid[_]]
  }

  it should "fail to extract a decimal string" in {
    extract(Map(fieldName -> "123.45")) shouldBe an[Invalid[_]]
  }

}
