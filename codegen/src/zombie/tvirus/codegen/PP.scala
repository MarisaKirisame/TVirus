package zombie.tvirus.codegen

import zombie.tvirus.parser.*
import zombie.tvirus.prettier.*
import Description.*

def bracket(x: String) = {
  "(" + x + ")"
}

def cbracket(x: String) = {
  "{" + x + "}"
}

def dbracket(x: Description) = {
  Text("(") <> x <> Text(")")
}

def dcbracket(x: Description) = {
  Text("{") <> x <> Text("}")
}

def dsep(x: Seq[Description], y: Description) = {
  x.flatMap(d => List(d, y))
    .dropRight(1)
    .foldLeft(Text(""))(Concat(_, _))
}

def dcsep(x: Seq[Description]) = dsep(x, Text(", "))

def dnsep(x: Seq[Description]) = dsep(x, Text("\n"))

def pp_type(x: Type): Description = {
  resolve(x) match {
    case Type.Var(name, _) => Text(name)
    case Type.App(f, y) =>
      dbracket(pp_type(f) <> dbracket(dcsep(y.map(pp_type))))
    case Type.Func(l, r) =>
      dbracket(dcsep(l.map(pp_type))) <> pp_type(r)
    case Type.TyCons(x) => Text(x)
    case Type.TypeScheme(xs, y) =>
      Text(s"forall ${xs.mkString(" ")}," ) <> pp_type(y)
    case Type.Prim(PrimType.INT)  => Text("int")
    case Type.Prim(PrimType.BOOL) => Text("bool")
  }
}

def pp_cbind(x: CBind): Description = {
  Text(x.name) <> Text(" ") <> dcsep(x.args.map(pp_type))
}

def pp_typedecl(x: TypeDecl): Description = {
  Text(x.name) <> Text(" ") <> Text(x.xs.mkString(", ")) <> Text(" = ") <> dnsep(x.cons.map(pp_cbind))
}

def pp_pat(x: Pat): Description = {
  x match
    case Pat.Wildcard => Text("_")
    case Pat.Cons(name, args) =>
      Text(s"$name ") <> dbracket(dcsep(args.map(pp_pat(_))))
    case Pat.Var(x) => Text(x)
}

def pp_binding(bind: (String, Expr)): Description = {
  Text(bind(0)) <> Text(" = ") <> pp_expr(bind(1))
}

def pp_op(op: PrimOp): Description = {
  op match
    case PrimOp.EQ    => Text("==")
    case PrimOp.ADD   => Text("+")
    case PrimOp.MINUS => Text("-")
    case PrimOp.LT    => Text("<")
    case PrimOp.GT    => Text(">")
    case PrimOp.DIV   => Text("/")
    case PrimOp.MOD   => Text("%")
}

def pp_expr(x: Expr): Description = {
  x match
    case Expr.Abs(xs, b) =>
      dbracket(
        Text("\\") <> Text(xs.mkString(" ")) <> Text(" -> ") <> pp_expr(b)
      )
    case Expr.App(f, y) =>
      dbracket(pp_expr(f) <> dbracket(dcsep(y.map(pp_expr))))
    case Expr.Var(y)       => Text(y)
    case Expr.InlineVar(y) => Text(y)
    case Expr.Match(y, cases) =>
      dbracket(
        Text("match ") <> pp_expr(y) <> Text(" with ") <> cases
          .map(z =>
            Nest(
              2,
              pp_pat(z(0)) <> Text(" => ") <>
                pp_expr(z(1))
            )
          )
          .flatMap(z => List(z, Text("\n| ")))
          .foldLeft(Text(""))(Concat(_, _))
      )
    case Expr.Cons(cons, xs) =>
      Text(cons) <> dbracket(dcsep(xs.map(pp_expr)))
    case Expr.Let(binding, body) =>
      Text("let ") <> dcsep(binding.map(pp_binding)) <> Text(" in ") <> pp_expr(body)
    case Expr.LitInt(y)      => Text(y.toString)
    case Expr.Prim(l, op, r) => pp_expr(l) <> pp_op(op) <> pp_expr(r)
    case Expr.LitBool(x) =>
      if (x) { Text("True") }
      else { Text("False") }
    case Expr.If(i, t, e) =>
      Text("if ") <> pp_expr(i) <> dcbracket(pp_expr(t)) <> Text(
        "else"
      ) <> dcbracket(pp_expr(e))
    case Expr.Fail() => Text("fail")
}

def pp_valdecl(x: ValueDecl): Description = {
  Text(x.x) <> Text(" = ") <> pp_expr(x.b)
}

def pp(x: Program): Description = {
  dnsep(x.tds.map(pp_typedecl)) <>
  dnsep(x.vds.map(pp_valdecl))
}

def show(x: Description): String = {
  layout(best(80, x))
}
