package pp202002.hw2
import pp202002.hw2.Data._
import scala.annotation.tailrec

object Main {

  /* Problem 1: Structural Sub Type
   Complete the definition of MyClass
   DO NOT USE "Any" in anywhere in your code.
   */
  object Problem1{
    class MyClass[A,B,C,D,E,F]() {
      type Func1 = { val a: A } => { val b: B }
      type Func2 = { val b: B } => { val a: A }
      type Func3 = { val c: C } => { val a: A }
      type Func4 = { val f: F } => { val d: D }

      type Ty1 = {
        def apply: { val func: Func1 ; val c: C } => { val b: B ; val d: D }
        def function1: { val func: Func3 } => { val c: C }
        val a: A
        val b: B
        val d: D
      }

      type Ty2 = {
        def apply: { val func: Func2 ; val e: E } => { val b: B ; val f: F ; val c: C }
        def function1: { val func: Func4 } => { val c: C; val e:E }
        val a: A
        val c: C
        val d: D
      }

      /*
       Find suitable common supertype of Ty1 and Ty2,
       and replace "Any" with that type.
       */
      // type CommonTy = Any // Any

      type CommonTy = {
        def apply: { val func: {} => {val a:A; val b:B}; val c:C; val e:E} => {val b:B}
        def function1: { val func: {} => {val a:A; val d:D} } => { val c: C }
        val a: A
        val d: D
      }
    }
  }

  /*
    Exercise 2: Regular Expressions

    Regular expression is a sequence of character that defines a pattern.
    In this exercise, we will cover two problems:
    (1) Converts a string to regular expression defined below.
    (2) Check if a regular expression matches given string.

    For more information on regular expressions, refer to the link below.
    https://en.wikipedia.org/wiki/Regular_expression

    (*) Syntax of Exp (see Data.scala)
    E -> a      (single alphanumeric character)
       | (EE)   (concatenation)
       | (E|E)  (or)
       | (E)*   (repetition / kleene star)
   */

  /*
    Exercise 2-1: Regex Converter
    Implement a function that converts a list of characters into a regular expression (Exp).
    The result of the conversion should be a regular expression (Exp) that means the same pattern as the input.

    To solve this problem, implement the two functions described below:
    (1) Function that convert list of character to list of token (lexer)
    (2) Function that convert list of token to regular expression (parser)

    (Tip) About Lexer
    Lexer is a function that divides a string into appropriate units, and the unit is called token.
    Token is defined in Data.scala, but there is no content yet.
    It is recommended to start by defining an appropriate Token to divide the String into.
    Information about lexers and tokens can be obtained from the link below.
    https://en.wikipedia.org/wiki/Lexical_analysis

    ex)
      converter({'(', '(', '2', '3', ')', ')', '*'})
        => EStar( EConcat( EChar('2') , EChar('3') ) )
      converter({'(', '1', '(', '(', '(', '2', '3', ')', ')', '*', '4', ')', ')'})
        => EConcat( EChar('1') , EConcat( EStar( EConcat( EChar('2') , EChar('3') ) ) , EChar('4') ) ) )

    (caution)
    1. Only '0' ~ '9', 'a' ~ 'z', 'A' ~ 'Z', '|', '*', '(', and ')' will be given as an input.
    2. Converter must be able to handle gramatical errors of input.
       Note that the result of converting an erroneous character list should be Error
       itself, rather than an expression containing Error.
       ex)
       (ab)* => EError()    [((ab))* is right expression]
       (((34) => EError()

    (*) "List" is an algebraic datatype which is defined as below.
      sealed abstract class List[A]
      case object Nil extends List[A]
      case class Cons(hd: A, tl: List[A]) extends List[A]

    Scala provides a special syntax for List.
    - "case object" can drop out "()" after the name of case class (like "Nil()")
    - "hd :: tl" means "Cons(hd, tl)".
    For usage, see below "drop_two".
      def drop_two(l: List[Int]): List[Int] =
        l match {
          case hd1::tl1 =>
            tl1 match {
              case hd2::tl2 => tl2
              case Nil => Nil
            }
          case _ => Nil

   ref) Technically, Scala can use special symbols as a valid identifier, so it defines "case class ::" instead "case class Cons".
        Therefore, you can't match List like "case Cons(hd, tl) => ..."
   */

  def lexer(l: List[Char]): List[Token] =
    l match {
      case h :: t =>
        h match {
          case '(' => TkLPar :: lexer(t)
          case ')' => TkRPar :: lexer(t)
          case '*' => TkStar :: lexer(t)
          case '|' => TkOr :: lexer(t)
          case _ if h.isLetterOrDigit => TkChar(h) :: lexer(t)
          case _ => lexer(t) // ignore other characters
        }
      case _ => Nil
    }


  def parseSingle(l: List[Token]): (Exp, List[Token]) =
    l match {
      case TkChar(c) :: t => (EChar(c), t)
      case TkLPar :: t =>
        parseSingle(t) match {
          case err@(EError, _) => err
          case (e1, TkRPar :: TkStar :: t2) => (EStar(e1), t2) // kleene
          case (e1, TkOr :: t2) => parseSingle(t2) match { // or
            case err@(EError, _) => err
            case (e2, TkRPar :: t3) => (EOr(e1, e2), t3)
            case _ => (EError, Nil)
          }
          case (e1, t2) => parseSingle(t2) match { // concat
            case err@(EError, _) => err
            case (e2, TkRPar :: t3) => (EConcat(e1, e2), t3)
            case _ => (EError, Nil)
          }
        }
      case _ => (EError, Nil)
    }

  def parser(l: List[Token]): Exp = {
    if (l.isEmpty) EEpsilon else {
      parseSingle(l) match {
        case (e, Nil) => e
        case _ => EError
      }
    }
  }

  def converter(l: List[Char]): Exp = {
    parser(lexer(l))
  }

  /*
    Exercise 2-2: Regex Matcher by Brzozowski derivative
    Implement a function that checks if a given regular expression matches a string.
    Since regular expression means a string pattern, you can determine whether a string belongs to the pattern.
    Use the "Brzozowski derivative" shown in the link below to implement a function
    that determines whether a string is included in the pattern of a regular expression.
    e.g.)
      matcher(converter("((23))*".toList), "2323232323") = true
      matcher(converter("((23))*".toList), "d") = false
      matcher(converter("(1(((23))*4))".toList), "12323234") = true

    https://en.wikipedia.org/wiki/Brzozowski_derivative
    https://www.ccs.neu.edu/home/turon/re-deriv.pdf
   */

  def containsEpsilon(e: Exp): Boolean = {
    e match {
      case EChar(_) => false
      case EStar(_) => true
      case EConcat(e1, e2) => containsEpsilon(e1) && containsEpsilon(e2)
      case EOr(e1, e2) => containsEpsilon(e1) || containsEpsilon(e2)
      case EEpsilon => true
      case EError => false
    }
  }

  def derivative(e: Exp, c: Char): Exp = {
    e match {
      case EChar(c1) => {
        if (c == c1) EEpsilon
        else EError
      }
      case EEpsilon => EError
      case EError => EError
      case EStar(e1) => EConcat(derivative(e1, c), EStar(e1))
      case EConcat(e1, e2) => {
        if (containsEpsilon(e1)) {
          EOr(EConcat(derivative(e1, c), e2), derivative(e2, c))
        } else {
          EConcat(derivative(e1, c), e2)
        }
      }
      case EOr(e1, e2) => EOr(derivative(e1, c), derivative(e2, c))
    }
  }

  @tailrec
  def matchList(e: Exp, str: List[Char]): Exp = {
    str match {
      case hd :: tl => matchList(derivative(e, hd), tl)
      case Nil => e
    }
  }

  def matcher(e: Exp, s: String): Boolean = {
    containsEpsilon(matchList(e, s.toList))
  }
}
