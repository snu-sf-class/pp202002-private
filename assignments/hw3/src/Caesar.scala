package pp202002.hw3

/*
  Exercise 2: Caesar cipher

  Implement Caesar cipher encryptor and decryptor.

  Caesar cipher shifts an alphabet to some constant value.
  If shift value is 3, 'A' is encoded to 'D', and 'Y' is encoded to 'B'.
  Conversely, 'C' is decoded to 'Z', 'F' is decoded to 'C', and so on.

  See https://en.wikipedia.org/wiki/Caesar_cipher

  The input will be restricted to upper cased alphabets. ('A'-'Z')
 */

object Caesar extends CipherGen[Int] {
  /** Makes new encoder
   *
   * @param initSetting shifted value (0 <= initSetting < 26)
   * @return new Caesar cipher encryptor
   */
  def buildEncryptor(initSetting: Int): CaesarEncryptor = ???

  /** Makes new decoder
   *
   * @param initSetting shifted value (0 <= initSetting < 26)
   * @return new Caesar cipher decryptor
   */
  def buildDecryptor(initSetting: Int): CaesarDecryptor = ???
}

class CaesarEncryptor(/* WRITE YOUR CODE */) extends Encryptor {
  def encrypt(c: Char): (Char, CaesarEncryptor) = ???
}

class CaesarDecryptor(/* WRITE YOUR CODE */) extends Decryptor {
  def decrypt(c: Char): (Char, CaesarDecryptor) = ???
}
