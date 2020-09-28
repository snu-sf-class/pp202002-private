package pp202002.hw1test
import pp202002.hw1.Main._

object Test extends App {
  def print_result(b:Boolean) : Unit =
    if (b) println("O") else println("X")

  // Problem 1
  print_result(collatz(5) == (6, 36))
  print_result(collatz(20) == (8, 66))
  print_result(collatz(6171) == (262, 12152173))


  // Problem 2
  // indefinite integral of x^2 is (x^3)/3.
  def intSquare(x: Float) = x * x * x / 3
  print_result(math.abs(integral(x => x*x, 0, 10) - (intSquare(10) - intSquare(0))) <= 0.001)

  // indefinite integral of cos(x) is sin(x)
  print_result(math.abs(integral(x => math.cos(x), 1, 5) - (math.sin(5) - math.sin(1))) <= 0.001)


  // Problem 3
  print_result(ppa((a, b) => a + b, 100, 200) == 20100)
  print_result(ppa((a, b) => 3*a*a - b, 20, 10) == 347099274)
}
