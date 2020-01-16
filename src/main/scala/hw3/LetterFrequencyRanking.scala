package hw3

object LetterFrequencyRanking {
  def apply(corpus: String): String = {
    val sortedLowerCaseLetters = corpus
      .toLowerCase()
      .toList
      .filter(char => char >= 'a' && char <= 'z')
      .sortWith((a, b) => a < b)
    val frequencies =
      getFrequencies(sortedLowerCaseLetters).sortWith((first, second) => {
        if (first._2 == second._2)
          first._1 < second._1
        else
          first._2 > second._2
      })
    frequencies.map(_._1).foldLeft("")((string, char) => string + char)
  }

  private def getFrequencies(letters: List[Char]): List[(Char, Int)] = {
    def inner(prevChar: Char,
              frequency: Int,
              letters: List[Char],
              acc: List[(Char, Int)]): List[(Char, Int)] = {
      if (letters.isEmpty)
        (prevChar, frequency) :: acc
      else if (prevChar == letters.head)
        inner(prevChar, frequency + 1, letters.tail, acc)
      else
        inner(letters.head, 1, letters.tail, (prevChar, frequency) :: acc)
    }

    if (letters.isEmpty)
      List.empty
    else
      inner(letters.head, 1, letters.tail, List())
  }

}
