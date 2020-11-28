package pp202002.project.common

object Environment {
  // add laziness to value
  sealed trait LazyVal[Env, V]
  case class LVVal[Env, V](v: V) extends LazyVal[Env, V] // primitive value
  case class LVLazy[Env, V](
      expr: Expr,
      env: Env,
      var evaluated: Option[V]
  ) extends LazyVal[Env, V] // lazy value

  trait EnvOps[Env, V] {
    def emptyEnv(): Env
    def pushEmptyFrame(env: Env): Env
    def popFrame(env: Env): Env
    def setItem(env: Env, name: String, item: LazyVal[Env, V]): Env
    def findItem(env: Env, name: String): Option[LazyVal[Env, V]]
  }

  type Frame[V] = Map[String, V]
  class MapEnv(val frames: List[Frame[LazyVal[MapEnv, Value[MapEnv]]]])

  type EnvVal = LazyVal[MapEnv, Value[MapEnv]]
}
