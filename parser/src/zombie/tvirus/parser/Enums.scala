package zombie.tvirus.parser

enum PrimOp:
  case ADD, MINUS, MUL, DIV, MOD, EQ, NE, GT, LT, GE, LE, STRAPPEND, STRLEN, STRHD, STRTL, STRCONS

enum PrimType:
  case INT, BOOL, CHAR, STRING

enum Type:
  case Prim(t: PrimType)
  case Func(xs: Seq[Type], r: Type)
  case Var(name: String, var ty: Option[Type])
  case App(f: Type, xs: Seq[Type])
  case TyCons(name: String)
  case TypeScheme(xs: Seq[String], t: Type)

enum Scheme:
  case Poly(xs: Seq[String], t: Type)
  case Mono(t: Type)

case class CBind(name: String, args: Seq[Type])

case class TypeDecl(name: String, xs: Seq[String], cons: Seq[CBind])

enum Pat:
  case Wildcard
  case Var(name: String)
  case Cons(name: String, arg: Seq[Pat])

// a simple constructor pattern. no nesting allowed
case class SCons(name: String, narg: Int)

enum Expr:
  case Prim(left: Expr, op: PrimOp, right: Expr)
  case Var(name: String)
  case InlineVar(name: String)
  case LitInt(inner: Int)
  case LitBool(inner: Boolean)
  case LitString(inner: String)
  case App(f: Expr, xs: Seq[Expr])
  case Abs(xs: Seq[String], b: Expr)
  case Let(xs: Seq[(String, Expr)], b: Expr)
  case Match(x: Expr, bs: Seq[(Pat, Expr)])
  case Cons(name: String, args: Seq[Expr])
  case DeclValue(t: Type)
  case If(i: Expr, t: Expr, e: Expr)
  // must have () because each Fail() bind to different type
  case Fail()

case class ValueDecl(x: String, b: Expr)

case class Program(tds: Seq[TypeDecl], vds: Seq[ValueDecl])
