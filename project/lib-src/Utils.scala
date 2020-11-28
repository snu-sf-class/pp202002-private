package pp202002.project.common

import pp202002.project.common.Environment._

object Utils {

  trait Show[T] {
    def show(t: T): String
  }

  def show[A](a: A)(implicit sh: Show[A]): String = sh.show(a)
  def showIter[A](al: Iterable[A])(implicit sh: Show[A]): String =
    al.map(sh.show).mkString(" ")

  implicit val showArg: Show[Arg] = "<precompiled binary>"
  implicit val showBind: Show[Bind] = "<precompiled binary>"
  implicit val showExpr: Show[Expr] = "<precompiled binary>"
  implicit def showValue[Env]: Show[Value[Env]] = "<precompiled binary>"
}
