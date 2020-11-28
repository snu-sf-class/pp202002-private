package pp202002.project.common

import scala.util.Try

trait Interpreter[E, V] {
  def interp(expr: E): Try[V]
}
