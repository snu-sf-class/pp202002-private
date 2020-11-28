package pp202002.project.common

sealed trait Arg
case class AVName(x: String) extends Arg // call by value
case class ANName(x: String) extends Arg // call by name

sealed trait Bind
case class BDef(f: String, params: List[Arg], body: Expr) extends Bind
case class BVal(x: String, e: Expr) extends Bind
case class BLVal(x: String, e: Expr) extends Bind

sealed trait Expr
case class EInt(n: Int) extends Expr
case class EName(x: String) extends Expr
case class EInL(e: Expr) extends Expr
case class EInR(e: Expr) extends Expr
case class EMatch(
    value: Expr,
    lvName: String,
    lvCase: Expr,
    rvName: String,
    rvCase: Expr
) extends Expr
case object ENil extends Expr
case class ECons(head: Expr, tail: Expr) extends Expr
case class EFst(e: Expr) extends Expr
case class ESnd(e: Expr) extends Expr
case class EApp(f: Expr, args: List[Expr]) extends Expr
case class ELet(bindings: List[Bind], e: Expr) extends Expr
case class ENilP(e: Expr) extends Expr
case class EIntP(e: Expr) extends Expr
case class ESumP(e: Expr) extends Expr
case class EProdP(e: Expr) extends Expr
case class EPlus(left: Expr, right: Expr) extends Expr
case class EMinus(left: Expr, right: Expr) extends Expr
case class EMul(left: Expr, right: Expr) extends Expr
case class EDiv(left: Expr, right: Expr) extends Expr
case class EMod(left: Expr, right: Expr) extends Expr
case class EEq(left: Expr, right: Expr) extends Expr
case class ELt(left: Expr, right: Expr) extends Expr
case class EGt(left: Expr, right: Expr) extends Expr
