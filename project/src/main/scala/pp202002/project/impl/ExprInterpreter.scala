package pp202002.project.impl

import pp202002.project.common._
import pp202002.project.common.Environment._

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

object ExprInterpreter {
  class InterpreterException(val reason: String) extends Exception {
    override def getMessage: String = reason
  }

  implicit def exprInterpreter[Env](implicit
      envOps: EnvOps[Env, Value[Env]]
  ): Interpreter[Expr, Value[Env]] = new Interpreter[Expr, Value[Env]] {

    def interp(expr: Expr): Try[Value[Env]] = ???

  }
}
