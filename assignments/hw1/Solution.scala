package pp202002.hw1
import scala.annotation.tailrec
import scala.util.control.TailCalls._

object Main {
  /* Implement given functions, which is currently blank. (???) */

  /*
    **WARNING: Please read the restrictions below carefully.**
    If you do not follow it, **your submission will not be graded.**
    1. Do not use keyword `var`. Use `val` and `def` instead.
    2. Do not use any library functions or data structures like `List`, `Array` `Range` (`1 to n`, `1 until n` ...), 
       `fold`, `map`, `reduce` or etc... You can only use tuples, `scala.annotation.tailrec`, and 
       `scala.util.control.TailCalls._` from the library.
    Again, if you do not follow these rules, your score will be zero.
    For all three problems, 50% of the test cases will require tail call optimizations (ie, have large inputs) 
    and the other 50% will not (ie, have small inputs).
    So, we will get 50% of the score if you submit a correct program without tail call optimization.
   */

  /*
   Exercise 1: Collatz sequence
   Consider this function: for a positive integer n,
   f(n) = n / 2       (if n is even)
   f(n) = 3 * n + 1   (if n is odd)
   Collatz sequence is defined as follows:
   c_0 = n  (n is positive integer)
   c_k = f(c_(k-1))
   Collatz conjecture(https://en.wikipedia.org/wiki/Collatz_conjecture) is the famous open problem,
   that no matter what value of starting value 'n', the sequence will always reach 1.
   Given a number n (1 <= n <= 10^12), compute the length and the sum of collatz sequence
   starting with n, until it reaches 1.
   e.g.) collatz(5) == (6, 36)
         (5 -> 16 -> 8 -> 4 -> 2 -> 1)
   */
  def collatz(n: Long): (Long, Long) = {
    @tailrec
    def ctr(n: Long, len: Long, sum: Long): (Long, Long) = {
      if (n == 1) (len, sum) else ctr(if (n % 2 == 0) n / 2 else n * 3 + 1, len + 1, sum + n)
    }

    ctr(n, 1, 1)
  }

  /*
   Exercise 2: Definite integral by Riemann sum
   Implement definite integral:
   Given a continuous function f, and range [a, b], compute the definite integral value over the interval [a, b]
   The value should be calculated by Riemann sum. (https://terms.naver.com/entry.nhn?docId=2073839&cid=47324&categoryId=47324)
   Divide the interval [a, b] into n equal intervals, and gather the areas below function f through each interval.
   The difference between your result and the real integral value should be less than 0.001
   Hint: Double `n` until the value moves less then 0.001
   
   Addition 10/04: Assume that the difference with a Riemann sum and a real integral value will always be less than 0.001 when n >= 10^7
   */
  def integral(f: Double => Double, a: Double, b: Double): Double = {
    @tailrec
    def intSum(i: Int, n: Int, h: Double, sum: Double): Double =
      if (i >= n) sum else intSum(i + 1, n, h, sum + h * f(a + h * i))


    @tailrec
    def guess(n: Int, value: Double): Double = {
      val guessedValue = intSum(0, n, (b - a) / n, 0)
      if (math.abs(guessedValue - value) < 0.001) guessedValue else guess(n * 2, guessedValue)
    }

    guess(4, intSum(0, 2, (b - a) / 2, 0))
  }

  /*
   Exercise 3: Ping-pong Ackermann function
   Given a function p and positive integer a and b, implement this function `ppa`: (0 <= a, b <= 10^6)
   ppa(p, a, b) = p(a, b)               (if a <= 0 or b <= 0)
   ppa(p, a, b) = p(a, ppa(p, a-1, b))  (if p(a, b) is even and a, b > 0)
   ppa(p, a, b) = p(ppa(p, a, b-1), b)  (if p(a, b) is odd and a, b > 0)
   Hint 1: If a, b > 0, wrap remained calculations, make it as an anonymous function and pass it to the parameter recursively.
           That anonymous function is called 'Continuation'. You'd better search 'Continuation Passing Style'.
   Hint 2: Due to the limits of Scala's platform (JVM), tail call (not tail recursion) is not optimized generally,
           especially when using continuation passing style.
           To optimize tail call properly, use `tailcall`, `done`, `result` functions from scala.util.control.TailCalls._
           (See https://stackoverflow.com/questions/16539488/why-scala-doesnt-make-tail-call-optimization)
   */
 def ppa(p: (Int, Int)=>Int, a: Int, b: Int): Int = {
   @tailrec
   def ppap(x: Int, y: Int, cont: Int=>TailRec[Int]): Int =
     if (x <= 0 || y <= 0) cont(p(x, y)).result
     else if (p(x, y) % 2 == 0) {
       ppap(x-1, y, (r) => tailcall(cont(p(x, r))))
     } else {
       ppap(x, y-1, (r) => tailcall(cont(p(r, y))))
     }

   ppap(a, b, (r) => done(r))
 }
}
