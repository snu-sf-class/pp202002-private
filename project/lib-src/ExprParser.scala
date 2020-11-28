package pp202002.project.common

import org.parboiled2._

import scala.annotation.switch

class ExprParser(val input: ParserInput) extends Parser {
  def Input: Rule1[Expr] = rule { WL ~ SExpr ~ WL ~ EOI }

  def SExpr: Rule1[Expr] = rule { SELiteral | '(' ~ WL ~ SEParens ~ WL ~ ')' }

  def SExprList: Rule1[List[Expr]] = rule {
    (SExpr ~ (&(ch(')') | EOI) | SP)).* ~> ((el: Seq[Expr]) => el.toList)
  }

  def SELiteral: Rule1[Expr] = rule {
    (str("nil") ~ push(ENil)) | (Integer ~> EInt) | (Ident ~> EName)
  }

  def SEParens: Rule1[Expr] = rule {
    run {
      (cursorChar: @switch) match {
        case 'i'                                           => SEInL | SEInR | SEIntP
        case 'm'                                           => SEMatch
        case 'c'                                           => SECons
        case 'f'                                           => SEFst
        case 's'                                           => SESnd | SESumP
        case 'a'                                           => SEApp
        case 'l'                                           => SELet
        case 'n'                                           => SENilP
        case 'p'                                           => SEProdP
        case '+' | '-' | '*' | '=' | '<' | '>' | '/' | '%' => SEBinOp
        case _                                             => MISMATCH
      }
    }
  }

  def SEInL: Rule1[EInL] = rule {
    atomic("inl") ~ SP ~ SExpr ~> EInL
  }
  def SEInR: Rule1[EInR] = rule {
    atomic("inr") ~ SP ~ SExpr ~> EInR
  }
  def SEMatch: Rule1[EMatch] = rule {
    atomic(
      "match"
    ) ~ SP ~ SExpr ~ SP ~ ('(' ~ WL ~ ParIdent ~ SP ~ SExpr ~ WL ~ ')') ~ SP ~
      ('(' ~ WL ~ ParIdent ~ SP ~ SExpr ~ WL ~ ')') ~> EMatch
  }
  def SECons: Rule1[ECons] = rule {
    atomic("cons") ~ SP ~ SExpr ~ SP ~ SExpr ~> ECons
  }
  def SEFst: Rule1[EFst] = rule {
    atomic("fst") ~ SP ~ SExpr ~> EFst
  }
  def SESnd: Rule1[ESnd] = rule {
    atomic("snd") ~ SP ~ SExpr ~> ESnd
  }
  def SEApp: Rule1[EApp] = rule {
    atomic("app") ~ SP ~ SExpr ~ SP ~ SExprList ~> EApp
  }
  def SELet: Rule1[ELet] = rule {
    atomic("let") ~ SP ~ SBindList ~ SP ~ SExpr ~> ELet
  }
  def SENilP: Rule1[ENilP] = rule {
    atomic("nil?") ~ SP ~ SExpr ~> ENilP
  }
  def SEIntP: Rule1[EIntP] = rule {
    atomic("int?") ~ SP ~ SExpr ~> EIntP
  }
  def SESumP: Rule1[ESumP] = rule {
    atomic("sum?") ~ SP ~ SExpr ~> ESumP
  }
  def SEProdP: Rule1[EProdP] = rule {
    atomic("prod?") ~ SP ~ SExpr ~> EProdP
  }
  def SEBinOp: Rule1[Expr] = rule {
    capture(anyOf("+-*/%=<>")) ~ SP ~
      SExpr ~ SP ~ SExpr ~> ((c: String, left: Expr, right: Expr) =>
        c match {
          case "+" => EPlus(left, right)
          case "-" => EMinus(left, right)
          case "*" => EMul(left, right)
          case "/" => EDiv(left, right)
          case "%" => EMod(left, right)
          case "=" => EEq(left, right)
          case "<" => ELt(left, right)
          case _   => EGt(left, right)
        }
      )
  }

  def SArg: Rule1[Arg] = rule {
    (Ident ~> AVName) | ('(' ~ WL ~ "lazy-val" ~ SP ~ Ident ~ WL ~ ")" ~> ANName)
  }

  def SArgList: Rule1[List[Arg]] = rule {
    '(' ~ WL ~ zeroOrMore(SArg ~ (&(')') | SP)) ~ ')' ~> ((args: Seq[Arg]) =>
      args.toList
    )
  }

  def SBind: Rule1[Bind] = rule {
    '(' ~ WL ~ (SBindDef | SBindVal | SBindLazyVal) ~ WL ~ ')'
  }

  def SBindList: Rule1[List[Bind]] = rule {
    '(' ~ WL ~ zeroOrMore(SBind ~ (&(')') | SP)) ~ ')' ~> ((args: Seq[Bind]) =>
      args.toList
    )
  }

  def SBindDef: Rule1[BDef] = rule {
    atomic("def") ~ SP ~ Ident ~ SP ~ SArgList ~ SP ~ SExpr ~> (
      (f: String, params: List[Arg], body: Expr) => BDef(f, params, body)
    )
  }

  def SBindVal: Rule1[BVal] = rule {
    atomic("val") ~ SP ~ Ident ~ SP ~ SExpr ~> BVal
  }

  def SBindLazyVal: Rule1[BLVal] = rule {
    atomic("lazy-val") ~ SP ~ Ident ~ SP ~ SExpr ~> BLVal
  }

  def Ident: Rule1[String] = rule {
    !CharPredicate.Digit ~ capture((CharPredicate.AlphaNum | '_').+)
  }

  def ParIdent: Rule1[String] = rule {
    '(' ~ WL ~ Ident ~ WL ~ ')'
  }

  def Integer: Rule1[Int] = rule {
    capture(ch('-').? ~ CharPredicate.Digit.+) ~> ((s: String) => s.toInt)
  }

  def WL: Rule0 = rule { quiet(anyOf(" \t\r\n").*) }
  def SP: Rule0 = rule { anyOf(" \t\r\n").+ }
}
