package pp202002.hw2test
import pp202002.hw2.Main._
import pp202002.hw2.Data._
import reflect.runtime.universe._

object Test extends App {
  def print_result(b:Boolean) : Unit =
    if (b) println("O") else println("X")

  // problem 1
  println("------Problem 1------");

  val mc = new Problem1.MyClass[Int, Int, Int, Int, Int, Int]()
  print_result(typeOf[mc.Ty1]<:<typeOf[mc.CommonTy]);
  print_result(!(typeOf[Any]<:<typeOf[mc.CommonTy]));

  val mc1 = new Problem1.MyClass[Int, Char, String, List[Int], List[Long], List[Char]]()
  print_result(typeOf[mc1.Ty1]<:<typeOf[mc1.CommonTy]);
  print_result(!(typeOf[Any]<:<typeOf[mc1.CommonTy]));

  // Problem 2
  println("------Problem 2------");

  println("------Problem 2-1 ------");
  print_result(converter("(a(b(cd)))".toList) == EConcat(EChar('a'),EConcat(EChar('b'),EConcat(EChar('c'),EChar('d')))))
  print_result(converter("(1(((23))*4))".toList) == EConcat(EChar('1'),EConcat(EStar(EConcat(EChar('2'),EChar('3'))),EChar('4'))))
  print_result(converter("((23))*".toList) == EStar(EConcat(EChar('2'),EChar('3'))))
  print_result(converter("(23)*".toList) == EError)
  print_result(converter("(C((bv))*)".toList) == EConcat(EChar('C'),EStar(EConcat(EChar('b'),EChar('v')))))
  print_result(converter("".toList) == EEpsilon)

  println("------Problem 2-2 ------");
  print_result(matcher(converter("(a(b(cd)))".toList), "abcd"))
  print_result(matcher(converter("(1(((23))*4))".toList), "14"))
  print_result(matcher(converter("((23))*".toList), "2323232323"))
  print_result(matcher(converter("(C((bv))*)".toList), "C"))
  print_result(matcher(converter("(C((bv))*)".toList), "Cbvbvbv"))

}