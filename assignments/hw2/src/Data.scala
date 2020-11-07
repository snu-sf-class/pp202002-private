package pp202002.hw2.Data

sealed abstract class Token
// WRITE YOUR CODE HERE

sealed abstract class Exp
case class EChar(c:Char) extends Exp
case class EStar(e: Exp) extends Exp
case class EConcat(e1: Exp, e2: Exp) extends Exp
case class EOr(e1:Exp, e2:Exp) extends Exp
case object EEpsilon extends Exp
case object EError extends Exp