package zombie.tvirus.codegen

import zombie.tvirus.parser.* 

def bracket(x: String) = {
  "(" + x + ")"
}

def pp_type(x: Type): String = {
  resolve(x) match {
    case Type.Var(name, _) => name
    case Type.App(f, x)    => bracket(pp_type(f) + " " + pp_type(x))
    case Type.Func(l, r) =>
      "(" + l.map(pp_type).mkString(", ") + ") -> " + pp_type(r)
    case Type.TyCons(x) => x
  }
}

def pp_cbind(x: CBind) = {
  x.name + " " + x.args.map(pp_type).mkString(" ")
}

def pp_typedecl(x: TypeDecl) = {
  x.name + " " + x.xs
    .mkString(",") + " = " + x.cons.map(pp_cbind).mkString("\n| ")
}

def pp_pat(x: Pat): String =
  x match
    case Pat.Wildcard         => "_"
    case Pat.Cons(name, args) => s"$name " + args.map(pp_pat(_)).mkString(" ")
    case Pat.Var(x)           => x

def pp_binding(bind: (String, Expr)): String = {
  bind(0) + " = " + pp_expr(bind(1))
}

def pp_expr(x: Expr): String = {
  x match {
    case Expr.Abs(xs, b) =>
      bracket("\\" + xs.mkString(" ") + " -> " + pp_expr(b))
    case Expr.App(f, x) =>
      bracket(pp_expr(f) + "(" + x.map(pp_expr).mkString(", ") + ")")
    case Expr.Var(x) => x
    case Expr.Match(x, cases) =>
      "match " + pp_expr(x) + " with " + cases
        .map(y => pp_pat(y(0)) + " => " + pp_expr(y(1)))
        .mkString("\n| ")
    case Expr.Cons(con, xs) => con + "(" + xs.map(pp_expr).mkString(", ") + ")"
    case Expr.Let(binding, body) =>
      "let " + binding.map(pp_binding).mkString(", ") + " in " + pp_expr(body)
  }
}

def pp_valdecl(x: ValueDecl) = {
  x.x + " = " + pp_expr(x.b)
}

def pp(x: Program) = {
  x.decls
    .map(y =>
      y match {
        case t: TypeDecl  => pp_typedecl(t)
        case v: ValueDecl => pp_valdecl(v)
      }
    )
    .mkString("\n") + "\n"
}