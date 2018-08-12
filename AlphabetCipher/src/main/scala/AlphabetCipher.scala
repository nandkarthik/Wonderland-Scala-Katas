object AlphabetCipher {
  val lookup = (0 to 25).map(r => (0 to 25).map(j => ((r + j) % 26 + 97).toChar))

  def encode(keyword: String, message: String): String = {
    message.zipWithIndex.map(r => encode(keyword.charAt(r._2 % keyword.length), r._1)).mkString("")
  }

  def decode(keyword: String, message: String): String = {
    message.zipWithIndex.map(r => decode(keyword.charAt(r._2 % keyword.length), r._1)).mkString("")
  }

  def decipher(cipher: String, message: String): String = {
    val result = cipher.zip(message).map(r => decipher(r._1, r._2)).mkString("")
    shorten(result)
  }

  def decipher(c: Char, m: Char) = {
    val mPos = m % 97

    val dec = lookup(mPos).zipWithIndex.filter(r => r._1 == c).head
    (dec._2 + 97).toChar
  }

  def decode(k: Char, m: Char) = {
    val kPos = k % 97

    val dec = lookup(kPos).zipWithIndex.filter(r => r._1 == m).head
    (dec._2 + 97).toChar
  }

  def encode(k: Char, m: Char) = {
    val kPos = k % 97
    val mPos = m % 97

    lookup(kPos)(mPos)
  }

  def shorten(str: String) = {
    (1 to str.length).map { len =>
      val sub = str.substring(0, len)
      val allSubs = str.grouped(len).toList
      val matchedCount = allSubs.count(r => matched(sub, r))
      if (matchedCount == allSubs.length) {
        Option(sub)
      }
      else {
        Option.empty
      }
    }.filter(r => r.isDefined).head.get
  }

  def matched(a: String, b: String) = {
    !a.zip(b).exists(r => r._1 != r._2)
  }
}
