package zombie.tvirus.codegen

import collection.mutable
import zombie.tvirus.parser.*

def is_atom(x: Expr): Boolean = {
  x match {
    case Expr.Var(_) => true
    case _           => false
  }
}

class LetSimplEnv {
  val defs = mutable.Map[String, Expr]()
  val use_count = mutable.Map[String, Int]()

  def can_remove(x: String): Boolean = {
    return (!defs.get(x).isEmpty) && (use_count(x) <= 1 || is_atom(defs(x)))
  }
}

def let_analysis(x: Expr, env: LetSimplEnv): Unit = {
  val recurse = x => let_analysis(x, env)
  x match {
    case Expr.Var(n) => {
      env.use_count.get(n) match {
        case None    => {}
        case Some(i) => env.use_count.put(n, i + 1)
      }
    }
    case Expr.Let(bindings, body) => {
      bindings.map((lhs, rhs) => {
        assert(env.defs.get(lhs).isEmpty)
        env.defs.put(lhs, rhs)
        env.use_count.put(lhs, 0)
        recurse(rhs)
      })
      recurse(body)
    }
    case Expr.Abs(args, body) => {
      recurse(body)
    }
    case Expr.Match(x, cases) => {
      recurse(x)
      cases.map((lhs, rhs) => recurse(rhs))
    }
    case Expr.App(f, xs) => {
      recurse(f)
      xs.map(recurse)
    }
    case Expr.Cons(name, xs) => {
      xs.map(recurse)
    }
    case Expr.LitInt(_) => {}
    case Expr.If(i, t, e) => {
      recurse(i)
      recurse(t)
      recurse(e)
    }
    case Expr.Prim(l, op, r) => {
      recurse(l)
      recurse(r)
    }
    case Expr.Fail() => {}
  }
}

def unlet(x: Expr, env: LetSimplEnv): Expr = {
  val recurse = x => unlet(x, env)
  val simp_name = (name: String) => {
    if (env.can_remove(name)) { Some(recurse(env.defs(name))) }
    else { None }
  }
  x match {
    case Expr.App(f, xs)          => Expr.App(recurse(f), xs.map(recurse))
    case Expr.Abs(bindings, body) => Expr.Abs(bindings, recurse(body))
    case Expr.Match(x, cases) =>
      Expr.Match(recurse(x), cases.map((lhs, rhs) => (lhs, recurse(rhs))))
    case Expr.Var(n) => {
      simp_name(n) match {
        case None    => Expr.Var(n)
        case Some(e) => recurse(e)
      }
    }
    case Expr.Let(binds, in) => {
      val bindings = binds
        .filter((lhs, rhs) => !env.can_remove(lhs))
        .map((lhs, rhs) => (lhs, recurse(rhs)))
      if (bindings.isEmpty) {
        recurse(in)
      } else {
        Expr.Let(bindings, recurse(in))
      }
    }
    case Expr.Cons(name, args) => Expr.Cons(name, args.map(recurse))
    case Expr.LitInt(_)        => x
    case Expr.If(i, t, e)      => Expr.If(recurse(i), recurse(t), recurse(e))
    case Expr.Prim(l, op, r)   => Expr.Prim(recurse(l), op, recurse(r))
    case Expr.Fail()           => Expr.Fail()
  }
}

def let_simplification(p: Program): Program = {
  assert(is_fresh(p))
  val env = LetSimplEnv()
  p.vds.map(vd => let_analysis(vd.b, env))
  Program(p.tds, p.vds.map(vd => ValueDecl(vd.x, unlet(vd.b, env))))
}
