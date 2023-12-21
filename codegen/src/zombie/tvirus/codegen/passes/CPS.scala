package zombie.tvirus.codegen.passes

import zombie.tvirus.codegen.CoreExpr

def base = "__cps_"
var count = 0

def freshName = {
  count = count + 1
  s"base$count"
}

def cps(expr: CoreExpr, k: CoreExpr): CoreExpr = expr match
  case CoreExpr.Lam(name, ty, body) => {
    val fk = freshName
    CoreExpr.App(
      k,
      CoreExpr.Lam(
        name,
        ty,
        CoreExpr.Lam(
          fk,
          ???, // FIXME
          cps(body, CoreExpr.Var(fk))
        )
      )
    )
  }
  case CoreExpr.App(fun, arg) => {
    val ff = freshName
    val fx = freshName
    cps(
      fun,
      CoreExpr.Lam(
        ff,
        ???,
        cps(
          arg,
          CoreExpr.Lam(
            fx,
            ???,
            CoreExpr.App(CoreExpr.App(CoreExpr.Var(ff), CoreExpr.Var(fx)), k)
          )
        )
      )
    )
  }

  case p @ CoreExpr.LitInt(inner)         => CoreExpr.App(k, p)
  case p @ CoreExpr.Prim(left, op, right) => CoreExpr.App(k, p)
  case p @ CoreExpr.Var(name)             => CoreExpr.App(k, p)
