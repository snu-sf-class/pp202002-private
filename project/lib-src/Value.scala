package pp202002.project.common

sealed trait Value[+Env]
case class VInt(n: Int) extends Value[Nothing]
case object VNil extends Value[Nothing] // product type nil
case class VCons[+Env](head: Value[Env], tail: Value[Env])
    extends Value[Env] // product type cons
case class VLeft[+Env](v: Value[Env]) extends Value[Env] // sum type left
case class VRight[+Env](v: Value[Env]) extends Value[Env] // sum type right
case class VFunc[+Env](
    funcName: String,
    params: List[Arg],
    body: Expr,
    env: Env
) extends Value[Env] // function as first-class value
