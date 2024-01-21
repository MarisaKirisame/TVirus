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

def pp_type(x: Type): Description = {
  resolve(x) match {
    case Type.Var(name, _) => Text(name)
    case Type.App(f, y) =>
      Text("(") <> pp_type(f) <> Text("(") <> y.map(pp_type).flatMap(d => List(d, Text(", "))).dropRight(1).foldLeft(Text(""))(Concat(_, _)) <> Text("))")
    case Type.Func(l, r) =>
      Text("(") <> l.map(pp_type).flatMap(d => List(d, Text(", "))).dropRight(1).foldLeft(Text(""))(Concat(_, _)) <> Text(")") <> pp_type(r)
    case Type.TyCons(x) => Text(x)
    case Type.TypeScheme(xs, y) => Text(s"forall ${xs.mkString(" ")}, ${pp_type(y)}") // what is TypeScheme?
    case Type.Prim(PrimType.INT) => Text("int")
    case Type.Prim(PrimType.BOOL) => Text("bool")
  }
}

def pp_cbind(x: CBind): Description = {
  Text(x.name) <> Text(" ") <> x.args.map(pp_type).flatMap(d => List(d, Text(" "))).dropRight(1).foldLeft(Text(""))(Concat(_, _))
}

def pp_typedecl(x: TypeDecl): Description = {
  Text(x.name) <> Text(" ") <> Text(x.xs.mkString(",")) <> Text(" = ") <> Text(x.cons.map(pp_cbind).mkString("\n| "))
}

def pp_pat(x: Pat): Description = {
  x match
    case Pat.Wildcard => Text("_")
    case Pat.Cons(name, args) => Text(s"$name " + args.map(pp_pat(_)).mkString(" "))
    case Pat.Var(x) => Text(x)
}

def pp_binding(bind: (String, Expr)): Description = {
  Text(bind(0)) <> Text (" = ") <> pp_expr(bind(1))
}

def pp_op(op: PrimOp): Description = {
  op match
    case PrimOp.EQ => Text("==")
    case PrimOp.ADD => Text("+")
    case PrimOp.MINUS => Text("-")
    case PrimOp.LT => Text("<")
    case PrimOp.GT => Text(">")
    case PrimOp.DIV => Text("/")
    case PrimOp.MOD => Text("%")
}

def pp_expr(x: Expr): Description = {
  x match
    case Expr.Abs(xs, b) =>
      Text("(") <> Text("\\") <> Text(xs.mkString(" ")) <> Text(" -> ") <> pp_expr(b) <> Text(")")
    case Expr.App(f, y) =>
      Text("(") <> pp_expr(f) <> Text("(") <> y.map(pp_expr).flatMap(d => List(d, Text(", "))).dropRight(1).foldLeft(Text(""))(Concat(_, _)) <> Text("))")
    case Expr.Var(y) => Text(y)
    case Expr.InlineVar(y) => Text(y)
    case Expr.Match(y, cases) =>
      Text("(match ") <> pp_expr(y) <> Text(" with ") <> cases.map(z => Nest(2, pp_pat(z(0)) <> Text(" => ") <>
                                                                  pp_expr(z(1)))).flatMap(z => List(z, Text("\n| "))).foldLeft(Text(""))(Concat(_, _)) <> Text(")")
    case Expr.Cons(cons, xs) => Text(cons) <> Text("(") <> xs.map(pp_expr).flatMap(d => List(d, Text(", "))).dropRight(1).foldLeft(Text(""))(Concat(_, _)) <> Text(")")
    case Expr.Let(binding, body) =>
      Text("let ") <> binding.map(pp_binding).flatMap(d => List(d, Text(", "))).dropRight(1).foldLeft(Text(""))(Concat(_, _)) <> Text(" in ") <> pp_expr(body)
    case Expr.LitInt(y) => Text(y.toString)
    case Expr.Prim(l, op, r) => pp_expr(l) <> pp_op(op) <> pp_expr(r)
    case Expr.LitBool(x) => if (x) { Text("True") } else { Text("False") }
    case Expr.If(i, t, e) =>
      Text("if ") <> pp_expr(i) <> Text("{") <> pp_expr(t) <> Text("}") <> Text("else") <> Text("{") <> pp_expr(e) <> Text("}")
    case Expr.Fail() => Text("fail")
}

def pp_valdecl(x: ValueDecl): Description = {
  Text(x.x) <> Text(" = ") <> pp_expr(x.b)
}

def pp(x: Program): Description = {
  x.tds.map(pp_typedecl).flatMap(d => List(d, Text("\n"))).dropRight(1).foldLeft(Text(""))(Concat(_, _)) <> Text("\n") <>
    x.vds.map(pp_valdecl).flatMap(d => List(d, Text("\n"))).dropRight(1).foldLeft(Text(""))(Concat(_, _))
}
