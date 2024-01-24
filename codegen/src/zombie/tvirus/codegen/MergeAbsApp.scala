package zombie.tvirus.codegen
import zombie.tvirus.parser.*
import zombie.tvirus.prettier.*

def merge_abs_app_expr(x: Expr, w: Watcher): Expr = {
  val recurse = x => merge_abs_app_expr(x, w)
  x match {
    case Expr.Var(_) | Expr.InlineVar(_) => x
    case Expr.Abs(bindings, body)        => Expr.Abs(bindings, recurse(body))
    case Expr.Match(x, cases) =>
      Expr.Match(x, cases.map((lhs, rhs) => (lhs, recurse(rhs))))
    case Expr.App(Expr.Abs(bindings, body), xs) => {
      w.make_progress()
      assert(bindings.length == xs.length)
      Expr.Let(bindings.zip(xs).map((b, x) => (b, recurse(x))), recurse(body))
    }
    case Expr.App(f, xs) => {
      Expr.App(recurse(f), xs.map(recurse))
    }
    case Expr.Cons(name, xs) => {
      Expr.Cons(name, xs.map(recurse))
    }
    case Expr.Let(bindings, body) => {
      Expr.Let(bindings.map((n, v) => (n, recurse(v))), recurse(body))
    }
    case Expr.LitInt(x) => {
      Expr.LitInt(x)
    }
    case Expr.LitBool(_) => x
    case Expr.If(i, t, e) => {
      Expr.If(recurse(i), recurse(t), recurse(e))
    }
    case Expr.Prim(l, op, r) => {
      Expr.Prim(recurse(l), op, recurse(r))
    }
    case Expr.Fail() => Expr.Fail()
  }
}

def merge_abs_app(p: Program, w: Watcher): Program = {
  Program(
    p.tds,
    p.vds.map(vd => ValueDecl(vd.x, merge_abs_app_expr(vd.b, w: Watcher)))
  )
}
