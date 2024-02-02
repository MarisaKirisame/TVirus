package zombie.tvirus.codegen
import zombie.tvirus.parser.*
import collection.mutable

var count = 0

def freshName() = {
  count = count + 1
  s"base$count"
}

def refresh_expr(x: Expr, remap: Map[String, String]): Expr = {
  val recurse = x => refresh_expr(x, remap)
  x match {
    case Expr.Var(n)       => Expr.Var(remap.get(n).getOrElse(n))
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
      Expr.Match(recurse(x), cases.map((lhs, rhs) => (lhs, recurse(rhs))))
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
  Program(p.tds, p.vds.map(vd => ValueDecl(vd.x, refresh_expr(vd.b, Map()))))
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
      recurse(x) && cases.forall((lhs, rhs) => recurse(rhs))
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
  }
}

def is_fresh(p: Program): Boolean = {
  val seen = mutable.Set[String]()
  p.vds.forall(vd => expr_is_fresh(vd.b, seen))
}