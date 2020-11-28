package pp202002.project.impl

import pp202002.project.common._
import pp202002.project.common.Environment._
import pp202002.project.impl.ExprInterpreter.InterpreterException

import scala.annotation.tailrec

object MapEnvImpl {
  implicit val mapEnvImpl: EnvOps[MapEnv, Value[MapEnv]] =
    new EnvOps[MapEnv, Value[MapEnv]] {
      def emptyEnv(): MapEnv = ???

      def pushEmptyFrame(env: MapEnv): MapEnv = ???

      def popFrame(env: MapEnv): MapEnv = ???

      def setItem(
          env: MapEnv,
          name: String,
          item: EnvVal
      ): MapEnv = ???

      def findItem(
          env: MapEnv,
          name: String
      ): Option[EnvVal] = ???
    }
}
