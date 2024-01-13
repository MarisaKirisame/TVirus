package zombie.tvirus.codegen
import zombie.tvirus.parser.*

def sum(x: Seq[Int]): Int = {
  x.foldLeft(0)((l, r) => l + r)
}

def size_pat(x: Pat): Int = {
  val recurse = x => size_pat(x)
  x match {
    case Pat.Var(_)     => 1
    case Pat.Cons(_, x) => 1 + sum(x.map(recurse))
  }
}

def size_case(x: (Pat, Expr)): Int = {
  size_pat(x(0)) + size_expr(x(1))
}

def size_expr(x: Expr): Int = {
  val recurse = x => size_expr(x)
  x match {
    case Expr.Var(_) | Expr.InlineVar(_) | Expr.LitInt(_) => 1
    case Expr.Abs(l, r)       => l.length + recurse(r)
    case Expr.App(f, x)       => recurse(f) + sum(x.map(recurse))
    case Expr.Match(x, cases) => recurse(x) + sum(cases.map(size_case))
    case Expr.Cons(_, x)      => 1 + sum(x.map(recurse))
    case Expr.Let(d, b)       => sum(d.map((l, r) => recurse(r))) + recurse(b)
    case Expr.If(i, t, e)     => recurse(i) + recurse(t) + recurse(e)
    case Expr.Prim(l, op, r)  => recurse(l) + 1 + recurse(r)
  }
}

def size(x: Program): Int = {
  sum(x.vds.map(vd => size_expr(vd.b)))
}

def simpl(p: Program): Program = {
  tyck_program(p)
  tyck_program(merge_abs_app(p))
  val new_p = let_simplification(merge_abs_app(p))
  if (size(new_p) < size(p)) { simpl(new_p) }
  else { p }
}
