package zombie.tvirus.parser

enum PrimOp:
  case ADD, MINUS, MUL, DIV, EQ, NE, GT, LT, GE, LE

enum PrimType:
  case INT, BOOL

enum Type:
  case Prim(t: PrimType)
  case Func(x: Type, r: Type)
  case Var(name: String)
  case App(f: Type, x: Type)

enum Scheme:
  case Poly(xs: Seq[String], t: Type)
  case Mono(t: Type)

case class TBind(name: String, t: Option[Type])
case class SBind(name: String, s: Option[Scheme])
case class CBind(name: String, args: Seq[Type])

case class TypeDecl(name: String, xs: Seq[String], cons: Seq[CBind])

enum Pat:
  case Var(name: String)
  case Wildcard
  case App(f: Pat, x: Pat)

enum Expr:
  case Prim(op: PrimOp)
  case Var(name: String)
  case LitInt(inner: Int)
  case App(f: Expr, x: Expr)
  case Abs(xs: Seq[TBind], b: Expr)
  case Let(xs: Seq[(SBind, Expr)], b: Expr)
  case Match(x: Expr, bs: Seq[(Pat, Expr)])

case class ValueDecl(x: SBind, b: Expr)

case class Program(decls: Seq[TypeDecl | ValueDecl])
