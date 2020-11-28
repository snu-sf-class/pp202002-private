package pp202002.project

import pp202002.project.common._
import pp202002.project.common.Environment._
import pp202002.project.common.Utils._
import pp202002.project.impl.MapEnvImpl._
import pp202002.project.impl.ExprInterpreter._

object Main extends App {
  ExprRepl.run[Value[MapEnv]]()
}
