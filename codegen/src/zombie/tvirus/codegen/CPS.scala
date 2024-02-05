package zombie.tvirus.codegen
import zombie.tvirus.parser.*

def cps_typedecl(x: TypeDecl) = {
  x
}

enum Cont:
  case HO(k: Expr => Expr)
  case FO(k: Expr)

  def toFO = {
    this match
      case HO(k) => hoas(k)
      case FO(k) => k
  }

  def toHO = {
    this match
      case HO(k) => k
      case FO(k) => (x: Expr) => Expr.App(k, Seq(x))
  }

def cps_expr_ho(x: Expr, k: Expr => Expr) = cps_expr(x, Cont.HO(k))

def cps_expr_fo(x: Expr, k: Expr) = cps_expr(x, Cont.FO(k))

def cps_exprs(x: Seq[Expr], k: Seq[Expr] => Expr): Expr = {
  x match {
    case Seq()   => k(Seq())
    case x +: xs => cps_expr_ho(x, x_ => cps_exprs(xs, xs_ => k(x_ +: xs_)))
  }
}

def cps_expr(x: Expr, k: Cont): Expr = {
  x match {
    case v @ Expr.Var(_) => k.toHO(v)
    case v @ Expr.GVar(_) => k.toHO(v)
    case Expr.Abs(xs, b) => {
      val k_ = freshName()
      k.toHO(
        Expr.Abs(
          xs :+ k_,
          cps_expr_fo(b, Expr.Var(k_))
        )
      )
    }
    case Expr.Match(x, cases) => {
      cps_expr_ho(
        x,
        x_ => Expr.Match(x_, cases.map((l, r) => (l, cps_expr(r, k))))
      )
    }
    case Expr.If(i, t, e) => {
        cps_expr_ho(i, i_ => Expr.If(i_, cps_expr(t, k), cps_expr(e, k)))
    }
    case Expr.App(f, x) => {
      cps_expr_ho(f, f_ => cps_exprs(x, x_ => Expr.App(f_, x_ :+ k.toFO)))
    }
    case Expr.Cons(name, args) => {
      cps_exprs(args, args_ => k.toHO(Expr.Cons(name, args_)))
    }
    case Expr.Let(bindings, body) => {
      if (bindings.isEmpty) {
        cps_expr(body, k)
      } else {
        val hbind = bindings.head
        cps_expr_fo(
          hbind(1),
          Expr.Abs(
            Seq(hbind(0)),
            cps_expr(Expr.Let(bindings.tail, body), k)
          )
        )
      }
    }
    case Expr.LitInt(_) => {
        k.toHO(x)
    }
    case Expr.LitBool(_) => {
      k.toHO(x)
    }
    case Expr.Prim(l, op, r) => {
        cps_expr_ho(l, l_ => cps_expr_ho(r, r_ => Expr.PrimCPS(l_, op, r_, k.toFO)))
    }
    case Expr.Fail() => k.toHO(Expr.Fail())
  }
}

def cps_valuedecl(x: ValueDecl) = {
  ValueDecl(x.x, cps_expr_ho(x.b, (y => y)))
}

def cps(p: Program): Program = {
  Program(p.tds.map(cps_typedecl), p.vds.map(cps_valuedecl))
}