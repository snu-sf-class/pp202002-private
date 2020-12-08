package pp202002.hw3

/*
  Exercise 1: Basic trait

  trait Encryptor and Decryptor forms cipher system.
  Both take an upper case alphabet ('A'-'Z') and return ciphered or deciphered upper case alphabet.

  Implement encrypt(s: String): (String, Encryptor) using encrypt(c: Char): (Char, Encryptor)
  also implement decrypt(s: String) by decrypt(c: Char)
 */

/** Abstract state of Encryptor */
trait Encryptor {
  /** Encrypts given character and returns a next state of the encryptor
   *
   * @param c a character to be encrypted
   * @return encrypted character and a next state of this encryptor
   */
  def encrypt(c: Char): (Char, Encryptor)

  /** Encrypts given string and returns a next state of the encryptor
   *
   * @param s a string to be encrypted
   * @return encrypted string and a next state of this encryptor
   */
  def encrypt(s: String): (String, Encryptor) = s.foldLeft(("", this))((enc, char) => {
    val (nextEnc, nextChar) = enc._2.encrypt(char)
    (enc._1 + nextEnc, nextChar)
  })
}


/** Abstract state of Decryptor */
trait Decryptor {
  /** Decrypts given character and returns a next state of the decryptor
   *
   * @param c a character to be decrypted
   * @return encrypted character and a next state of this decryptor
   */
  def decrypt(c: Char): (Char, Decryptor)

  /** Decrypts given string and returns a next state of the decryptor
   *
   * @param s a string to be decrypted
   * @return decrypted string and a next state of this decryptor
   */
  def decrypt(s: String): (String, Decryptor) = s.foldLeft(("", this))((dec, char) => {
    val (nextDec, nextChar) = dec._2.decrypt(char)
    (dec._1 + nextDec, nextChar)
  })
}

/** Cipher Generator */
trait CipherGen[T] {
  /** generates encryptor by initial settings */
  def buildEncryptor(initSetting: T): Encryptor

  /** generates decryptor by initial settings */
  def buildDecryptor(initSetting: T): Decryptor
}