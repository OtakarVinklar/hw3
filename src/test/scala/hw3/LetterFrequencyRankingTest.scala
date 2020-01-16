package hw3

import hw3.Main.letterFrequencyRanking
import org.scalatest.{FunSuite, Matchers}

class LetterFrequencyRankingTest extends FunSuite with Matchers {
  test("Simple") { LetterFrequencyRanking("hello") shouldBe "leho" }
  test("Capital letters") { LetterFrequencyRanking("AaaAaaAaa") shouldBe "a" }
  test("Punctuation") { LetterFrequencyRanking("Sic!") shouldBe "cis" }

  test("Empty1") { LetterFrequencyRanking("';.,`523542952!") shouldBe "" }
  test("Empty2") { LetterFrequencyRanking("") shouldBe "" }
  test("someOtherTest1") {
    LetterFrequencyRanking("abcddefgg") shouldBe "dgabcef"
  }
  test("someOtherTest2") {
    LetterFrequencyRanking("ZaAazZa") shouldBe "az"
  }
}
