import org.parboiled2.{ErrorFormatter, ParseError}
import org.scalatest.flatspec.AnyFlatSpec
import pp202002.project.common._
import pp202002.project.impl.MapEnvImpl._
import pp202002.project.impl.ExprInterpreter.exprInterpreter
import pp202002.project.impl.Prime.nthPrime

import scala.util.{Failure, Success}

class InterpreterTest extends AnyFlatSpec {
  def parse(str: String): Expr = {
    val parser = new ExprParser(str)
    val result = parser.Input.run()
    result match {
      case Success(v) => v
      case Failure(e: ParseError) =>
        throw new RuntimeException(
          parser.formatError(e, new ErrorFormatter(showTraces = true))
        )
    }
  }

  def runAssert[Env](code: String, expect: Value[Env]): Unit = {
    assert(exprInterpreter.interp(parse(code)) == Success(expect))
  }

  "Basic let" should "return 3" in {
    val adder =
      """
        |(let ((val x 1) (val y 2)) (+ x y))
        |""".stripMargin

    runAssert(adder, VInt(3))
  }

  "Basic def" should "return 1" in {
    val adder =
      """
        |(let ((def f (x) (+ x -1))) (app f 2))
        |""".stripMargin

    runAssert(adder, VInt(1))
  }

  "Basic let*" should "return 11" in {
    val adder =
      """
        |(let ((val x 3) (val y (+ x 5))) (+ x y))
        |""".stripMargin

    runAssert(adder, VInt(11))
  }

  "Basic def*" should "return 11" in {
    val adder =
      """
        |(let ((val x 3) (def f (y) (+ x y))) (+ x (app f 5)))
        |""".stripMargin

    runAssert(adder, VInt(11))
  }

  "Basic lazy val*" should "return 11" in {
    val adder =
      """
        |(let ((val x 3) (lazy-val y (+ x 5))) (+ x y))
        |""".stripMargin

    runAssert(adder, VInt(11))
  }

  "Basic product type" should "return (4 . 1)" in {
    val code =
      """
        |(let ((val x 13) (val y 3)) (cons (/ x y) (% x y)))
        |""".stripMargin

    runAssert(code, VCons(VInt(4), VInt(1)))
  }

  "Basic sum type" should "return ((left 1) . (right 2))" in {
    val code =
      """
        |(let ((val x 1)) (cons (inl x) (inr (+ x 1))))
        |""".stripMargin

    runAssert(code, VCons(VLeft(VInt(1)), VRight(VInt(2))))
  }

  "Basic recursion" should "return 15" in {
    val adder =
      """
        |(let
        |    ((def sum (n)
        |          (match (< n 1)
        |                 ((_) (+ (app sum (- n 1)) n))
        |                 ((_) 0))
        |          ))
        |  (app sum 5))""".stripMargin

    runAssert(adder, VInt(15))
  }

  "Fibonacci(10)" should "return 55" in {
    val fib =
      """(let
        |    ((def fib (n)
        |          (match (< n 1)
        |                 ((_) (match (= n 1)
        |                             ((_) (+ (app fib (- n 1)) (app fib (- n 2))))
        |                             ((_) 1)
        |                             ))
        |                 ((_) 0))))
        |  (app fib 10))""".stripMargin

    runAssert(fib, VInt(55))
  }

  def getNthPrime(nthPrime: String, n: Int): String =  s"(let ((val fx $nthPrime)) (app fx $n))"

  "nthPrime" should "return nth-prime" in {
    runAssert(getNthPrime(nthPrime, 1), VInt(2))
    runAssert(getNthPrime(nthPrime, 2), VInt(3))
    runAssert(getNthPrime(nthPrime, 3), VInt(5))
    runAssert(getNthPrime(nthPrime, 4), VInt(7))
    runAssert(getNthPrime(nthPrime, 5), VInt(11))
    runAssert(getNthPrime(nthPrime, 10), VInt(29))
  }
}
