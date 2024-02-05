package zombie.tvirus.codegen
import zombie.tvirus.parser.*
import collection.mutable
import algebra.lattice.Bool

var count = 0

def freshName() = {
  count = count + 1
  s"base$count"
}

def refresh_pat(
    x: Pat,
    remap: Map[String, String]
): (Pat, Map[String, String]) = {
  x match
    case Pat.Wildcard => (Pat.Wildcard, remap)
    case Pat.Cons(n, args) => {
      val (new_args: Seq[Pat], new_remap: Map[String, String]) =
        args.foldLeft((Seq[Pat](), remap))((left, p) => {
          val res = refresh_pat(p, left(1))
          (left(0) :+ res(0), res(1))
        })
      (Pat.Cons(n, new_args), new_remap)
    }
    case Pat.Var(x) => {
      val f = freshName()
      (Pat.Var(f), remap + (x -> f))
    }
}

// careful! it might be tempting to make remap mutable to avoid plumbing,
// but that is incorrect - doing so will make it not respect scoping rule.
def refresh_expr(x: Expr, remap: Map[String, String]): Expr = {
  val recurse = x => refresh_expr(x, remap)
  x match {
    case Expr.Var(n)       => Expr.Var(remap.get(n).getOrElse(n))
    case Expr.GVar(n)      => Expr.GVar(n)
    case Expr.InlineVar(n) => Expr.InlineVar(remap.get(n).getOrElse(n))
    case Expr.Abs(bindings, body) => {
      // ensure there are not duplicate values in bindings
      assert(bindings.toSet.size == bindings.size)
      val (new_bindings, new_remap) =
        bindings.foldLeft((Seq[String](), remap))((res, next_name) => {
          val fn = freshName()
          (res(0) :+ fn, res(1) + (next_name -> fn))
        })
      Expr.Abs(new_bindings, refresh_expr(body, new_remap))
    }
    case Expr.Let(bindings, body) => {
      assert(bindings.toSet.size == bindings.size)
      val (new_bindings, new_remap) = bindings.foldLeft(
        (Seq[(String, Expr)](), remap)
      )((res, next_binding) => {
        val next_name = next_binding(0)
        val fn = freshName()
        (
          res(0) :+ (fn, refresh_expr(next_binding(1), res(1))),
          res(1) + (next_name -> fn)
        )
      })
      Expr.Let(new_bindings, refresh_expr(body, new_remap))
    }
    case Expr.Match(x, cases) => {
      Expr.Match(
        recurse(x),
        cases.map((lhs, rhs) => {
          val (new_lhs, new_remap) = refresh_pat(lhs, remap)
          (new_lhs, refresh_expr(rhs, new_remap))
        })
      )
    }
    case Expr.Cons(name, xs) => {
      Expr.Cons(name, xs.map(recurse))
    }
    case Expr.App(f, xs) => {
      Expr.App(recurse(f), xs.map(recurse))
    }
    case Expr.LitInt(x) => {
      Expr.LitInt(x)
    }
    case Expr.LitBool(inner) => x
    case Expr.Prim(l, op, r) => {
      Expr.Prim(recurse(l), op, recurse(r))
    }
    case Expr.If(i, t, e) => {
      Expr.If(recurse(i), recurse(t), recurse(e))
    }
    case Expr.Fail() => Expr.Fail()
  }
}

def refresh(p: Program): Program = {
  val ret =
    Program(p.tds, p.vds.map(vd => ValueDecl(vd.x, refresh_expr(vd.b, Map()))))
  assert(is_fresh(ret))
  ret
}

def pat_is_fresh(x: Pat, seen: mutable.Set[String]): Boolean = {
  val recurse = x => pat_is_fresh(x, seen)
  x match
    case Pat.Wildcard      => true
    case Pat.Cons(_, pats) => pats.forall(recurse)
    case Pat.Var(n) =>
      if (seen.contains(n)) {
        false
      } else {
        seen.add(n)
        true
      }
}

def expr_is_fresh(x: Expr, seen: mutable.Set[String]): Boolean = {
  val recurse = x => expr_is_fresh(x, seen)
  x match {
    case Expr.Var(_) | Expr.InlineVar(_) => true
    case Expr.Abs(bindings, body) => {
      var ret = true
      bindings.map(n => {
        if (seen.contains(n)) {
          ret = false
        } else {
          seen.add(n)
        }
      })
      ret && recurse(body)
    }
    case Expr.Match(x, cases) => {
      recurse(x) && cases.forall((lhs, rhs) =>
        pat_is_fresh(lhs, seen) && recurse(rhs)
      )
    }
    case Expr.Let(bindings, body) => {
      var ret = true
      bindings.map((n, rhs) => {
        if (seen.contains(n)) {
          ret = false
        } else {
          seen.add(n)
        }
      })
      ret && recurse(body)
    }
    case Expr.App(f, xs)     => recurse(f) && xs.forall(recurse)
    case Expr.Cons(name, xs) => xs.forall(recurse)
    case Expr.LitInt(_)      => true
    case Expr.LitBool(_)     => true
    case Expr.If(i, t, e)    => recurse(i) && recurse(t) && recurse(e)
    case Expr.Prim(l, op, r) => recurse(l) && recurse(r)
    case Expr.Fail()         => true
    case Expr.GVar(_)        => true
  }
}

def is_fresh(p: Program): Boolean = {
  val seen = mutable.Set[String]()
  p.vds.forall(vd => expr_is_fresh(vd.b, seen))
}
