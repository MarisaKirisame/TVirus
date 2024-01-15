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
    case Expr.Var(_) | Expr.InlineVar(_) | Expr.LitInt(_) | Expr.LitBool(_) => 1
    case Expr.Abs(l, r)       => l.length + recurse(r)
    case Expr.App(f, x)       => recurse(f) + sum(x.map(recurse))
    case Expr.Match(x, cases) => recurse(x) + sum(cases.map(size_case))
    case Expr.Cons(_, x)      => 1 + sum(x.map(recurse))
    case Expr.Let(d, b)       => sum(d.map((l, r) => recurse(r))) + recurse(b)
    case Expr.If(i, t, e)     => recurse(i) + recurse(t) + recurse(e)
    case Expr.Prim(l, op, r)  => recurse(l) + 1 + recurse(r)
    case Expr.Fail()          => 1
  }
}

def size(x: Program): Int = {
  sum(x.vds.map(vd => size_expr(vd.b)))
}

def unsimpl_expr(x: Expr): Expr = {
  val recurse = x => unsimpl_expr(x)
  x match {
    case Expr.InlineVar(n) => Expr.Var(n)
    case Expr.Abs(l, r)    => Expr.Abs(l, recurse(r))
    case Expr.Let(d, b) =>
      Expr.Let(d.map((l, r) => (l, recurse(r))), recurse(b))
    case Expr.App(f, x) => Expr.App(recurse(f), x.map(recurse))
    case Expr.Match(x, cases) =>
      Expr.Match(recurse(x), cases.map((l, r) => (l, recurse(r))))
    case Expr.Var(n)         => Expr.Var(n)
    case Expr.If(i, t, e)    => Expr.If(recurse(i), recurse(t), recurse(e))
    case Expr.Prim(l, op, r) => Expr.Prim(recurse(l), op, recurse(r))
    case Expr.Cons(name, xs) => Expr.Cons(name, xs.map(recurse))
    case Expr.LitInt(i)      => Expr.LitInt(i)
    case Expr.LitBool(_)     => x
    case Expr.Fail()         => Expr.Fail()
  }
}

def unsimpl(x: Program): Program = {
  Program(x.tds, x.vds.map(vd => ValueDecl(vd.x, unsimpl_expr(vd.b))))
}

def simpl(p: Program): Program = {
  val new_p = let_simplification(refresh(merge_abs_app(p)))
  if (size(new_p) != size(p)) {
    println("continue simplification!")
    simpl(new_p)
  } else {
    unsimpl(p)
  }
}
