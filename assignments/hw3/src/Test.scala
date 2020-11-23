package pp202002.hw3test
import pp202002.hw3._

object Test extends App {
  val caesarE = Caesar.buildEncryptor(23)
  println(caesarE.encrypt("THEQUICKBROWNFOXJUMPSOVERTHELAZYDOG")._1 == "QEBNRFZHYOLTKCLUGRJMPLSBOQEBIXWVALD")

  val caesarD = Caesar.buildDecryptor(23)
  println(caesarD.decrypt("QEBNRFZHYOLTKCLUGRJMPLSBOQEBIXWVALD")._1 == "THEQUICKBROWNFOXJUMPSOVERTHELAZYDOG")

  val qwertyWheel = Wire("QWERTYUIOPASDFGHJKLZXCVBNM")
  val revReflector = Wire("ZYXWVUTSRQPONMLKJIHGFEDCBA")
  val testSetting = EnigmaSettings(qwertyWheel :: Nil, revReflector)
  val testEnigmaEnc = Enigma.buildEncryptor(testSetting)
  val testEnigmaDec = Enigma.buildDecryptor(testSetting)
  println(testEnigmaEnc.encrypt("THEQUICKBROWNFOXJUMPSOVERTHELAZYDOG")._1 == "BGRNPHYUUKFEDHKUMWFLWHKFJKXOQEVFRYI")
  println(testEnigmaDec.decrypt("BGRNPHYUUKFEDHKUMWFLWHKFJKXOQEVFRYI")._1 == "THEQUICKBROWNFOXJUMPSOVERTHELAZYDOG")
                           
  // This is the real enigma machine settings used in WWII.
  val UKW_B_Reflector = Wire("YRUHQSLDPXNGOKMIEBFZCWVJAT")
  val wheel1 = Wire("EKMFLGDQVZNTOWYHXUSPAIBRCJ")
  val wheel2 = Wire("AJDKSIRUXBLHWTMCQGZNPYFVOE")
  val wheel3 = Wire("BDFHJLCPRTXVZNYEIWGAKMUSQO")
  val enigmaM3Settings = EnigmaSettings(wheel3 :: wheel2 :: wheel1 :: Nil, UKW_B_Reflector)
  val enigmaM3 = Enigma.buildEncryptor(enigmaM3Settings)

  // You can check your implementation from the site below.
  // https://www.101computing.net/enigma-machine-emulator/
  // As we do not implement Turnover notch position, 
  // the result of our enigma machine should be differed from 21th letter.
  println(enigmaM3.encrypt("THEQUICKBROWNFOXJUMPSOVERTHELAZYDOG")._1 == "OPCILLAZFXLQTDNLGGLEKWWTHKQKGXIEZKD")
  println(enigmaM3.decrypt("OPCILLAZFXLQTDNLGGLEKWWTHKQKGXIEZKD")._1 == "THEQUICKBROWNFOXJUMPSOVERTHELAZYDOG")
}
