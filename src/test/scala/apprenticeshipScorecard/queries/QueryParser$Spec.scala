package apprenticeshipScorecard.queries

import org.scalatest.{FlatSpec, Matchers}

class QueryParser$Spec extends FlatSpec with Matchers {

  import QueryAST._
  import QueryParser._
  import atto.Atto._
  import atto.ParseResult._

  "identifier" should "match a" in {
    identifier.parseOnly("a") shouldBe Done("", "a")
  }

  it should "match _a" in {
    identifier.parseOnly("_a") shouldBe Done("", "_a")
  }

  it should "match a3_b" in {
    identifier.parseOnly("a3_b") shouldBe Done("", "a3_b")
  }

  it should "not match 3a" in {
    identifier.parseOnly("2a") shouldBe a[Fail]
  }

  "path" should "match a" in {
    path.parseOnly("a") shouldBe Done("", Path(List("a")))
  }

  it should "match a.b" in {
    path.parseOnly("a.b") shouldBe Done("", Path(List("a", "b")))
  }

  "stringComparison" should """match a = "foo"""" in {
    stringComparison.parseOnly("""a = "foo"""") shouldBe Done("", SEQ(Path(List("a")), "foo"))
  }

  it should """match a != "foo"""" in {
    stringComparison.parseOnly("""a != "foo"""") shouldBe Done("", SNEQ(Path(List("a")), "foo"))
  }

  it should """match a starts-with "foo"""" in {
    stringComparison.parseOnly("""a starts-with "foo"""") shouldBe Done("", StartsWith(Path(List("a")), "foo"))
  }

  it should """match a ends-with "foo"""" in {
    stringComparison.parseOnly("""a ends-with "foo"""") shouldBe Done("", EndsWith(Path(List("a")), "foo"))
  }

  it should """match a contains "foo"""" in {
    stringComparison.parseOnly("""a contains "foo"""") shouldBe Done("", Contains(Path(List("a")), "foo"))
  }

  "numberComparison" should "match a = 3.0" in {
    numberComparison.parseOnly("a = 3.0") shouldBe Done("", EQ(Path(List("a")), 3.0))
  }
  it should "match a < 3" in {
    numberComparison.parseOnly("a < 3") shouldBe Done("", LT(Path(List("a")), 3.0))
  }
  it should "match a >  3" in {
    numberComparison.parseOnly("a >  3") shouldBe Done("", GT(Path(List("a")), 3.0))
  }
  it should "match a != 3.0" in {
    numberComparison.parseOnly("a != 3.0") shouldBe Done("", NEQ(Path(List("a")), 3.0))
  }

  it should "match a<=3.0" in {
    numberComparison.parseOnly("a<=3.0") shouldBe Done("", LE(Path(List("a")), 3.0))
  }
  it should "match a>=-3" in {
    numberComparison.parseOnly("a>=-3") shouldBe Done("", GE(Path(List("a")), -3))
  }

  "conjunction" should "match a = 3 and b = 2" in {
    conjunction.parseOnly("a = 3 and b = 2") shouldBe Done("", AND(EQ(Path(List("a")), 3), EQ(Path(List("b")), 2)))
  }
  it should "match a = 3 or b = 2" in {
    conjunction.parseOnly("a = 3 or b = 2") shouldBe Done("", OR(EQ(Path(List("a")), 3), EQ(Path(List("b")), 2)))
  }
  it should "match a = 3 and b = 2 or c = 5" in {
    conjunction.parseOnly("a = 3 and b = 2 or c = 5") shouldBe Done("", AND(EQ(Path(List("a")), 3.0), OR(EQ(Path(List("b")), 2.0), EQ(Path(List("c")), 5.0))))
  }

  it should "match (a = 3 and b = 2) or c = 5" in {
    conjunction.parseOnly("(a = 3 and b = 2) or c = 5") shouldBe Done("", OR(AND(EQ(Path(List("a")), 3.0), EQ(Path(List("b")), 2.0)), EQ(Path(List("c")), 5.0)))
  }

  it should "match a = 3 and (b = 2 or c = 5)" in {
    conjunction.parseOnly("a = 3 and (b = 2 or c = 5)") shouldBe Done("", AND(EQ(Path(List("a")), 3.0), OR(EQ(Path(List("b")), 2.0), EQ(Path(List("c")), 5.0))))
  }

  it should "match (a = 3 and b = 2) or (c = 5 and d = 6)" in {
    conjunction.parseOnly("(a = 3 and b = 2) or (c = 5 and d = 6)") shouldBe Done("", OR(AND(EQ(Path(List("a")), 3.0), EQ(Path(List("b")), 2.0)), AND(EQ(Path(List("c")), 5.0), EQ(Path(List("d")), 6.0))))
  }

  it should "match ((a = 3 and b = 2) or (c = 5 and d = 6)) and x = 9" in {
    conjunction.parseOnly("((a = 3 and b = 2) or (c = 5 and d = 6)) and x = 9") shouldBe
      Done("",
        AND(
          OR(
            AND(EQ(Path(List("a")), 3.0), EQ(Path(List("b")), 2.0)),
            AND(EQ(Path(List("c")), 5.0), EQ(Path(List("d")), 6.0))),
          EQ(Path(List("x")), 9.0)))
  }

  "query" should "match a = 3" in {
    query.parseOnly("a = 3") shouldBe Done("", EQ(Path(List("a")), 3.0))
  }
  it should "match a = 3 or b = 2" in {
    query.parseOnly("a = 3 or b = 2") shouldBe Done("", OR(EQ(Path(List("a")), 3.0), EQ(Path(List("b")), 2.0)))
  }
  it should "fail to match" in {
    // Checking that this does not put the parser into an infinite loop
    query.parseOnly("""a starts-ith "foo"""") shouldBe a[Fail]
  }
}
