package hw3

object GreyCode {
  val INT_BIT_SIZE = 32

  def nBitCode(bitCount: Int): List[String] = {
    assert(bitCount > 0)

    def inner(n: Int, maxN: Int, acc: List[String]): List[String] = {
      if (n == maxN)
        return acc

      val firstHalf = acc.map(number => '0' + number)
      val secondHalf = acc.reverse.map(number => '1' + number)

      inner(n + 1, maxN, firstHalf ++ secondHalf)
    }

    inner(1, bitCount, List("0", "1"))
  }

  private def prefixZeros(binNumber: String, bitCount: Int): String = {
    "0" * (bitCount - binNumber.length) ++ binNumber
  }
}
