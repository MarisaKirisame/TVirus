package zombie.tvirus.codegen
import org.antlr.v4.runtime.CharStreams
import java.io.FileWriter
import java.io.IOException
import zombie.tvirus.parser.*
import collection.mutable
import zombie.tvirus.prettier.*

class Watcher {
  var progress = true
  def make_progress() = {
    progress = true
  }

  def progress_made() = {
    val ret = progress
    progress = false
    ret
  }
}

def hoas(f: (Expr => Expr)): Expr = {
  val x_ = freshName()
  Expr.Abs(Seq(x_), f(Expr.Var(x_)))
}

def is_var(t: Type) = {
  t match {
    case Type.Var(_, _) => true
    case _              => false
  }
}

var count = 0

def freshName() = {
  count = count + 1
  s"base$count"
}

def consExpr(e: Expr, consDecls: Set[String]): Expr = {
  val recurse = x => consExpr(x, consDecls)
  e match
    case Expr.Prim(left, op, right) =>
      Expr.Prim(recurse(left), op, recurse(right))
    case Expr.App(f, xs) =>
      f match
        case Expr.Var(name) if consDecls.contains(name) =>
          Expr.Cons(name, xs.map(recurse))
        case _ => Expr.App(recurse(f), xs.map(recurse))
    case Expr.Abs(xs, b) => Expr.Abs(xs, recurse(b))
    case Expr.Let(xs, b) =>
      Expr.Let(xs.map((l, r) => (l, recurse(r))), recurse(b))
    case Expr.Match(x, bs) =>
      Expr.Match(
        recurse(x),
        bs.map(b => (b(0), recurse(b(1))))
      )
    case Expr.Var(n)      => Expr.Var(n)
    case Expr.LitInt(x)   => Expr.LitInt(x)
    case Expr.LitBool(x)  => e
    case Expr.If(i, t, e) => Expr.If(recurse(i), recurse(t), recurse(e))
}

def consType(x: Type, decls: Set[String]): Type = {
  val recurse = x => consType(x, decls)
  resolve(x) match {
    case v @ Type.Var(_, _) =>
      if (decls.contains(v.name)) { Type.TyCons(v.name) }
      else { v }
    case Type.App(f, xs) => Type.App(recurse(f), xs.map(recurse))
    case Type.Prim(t)    => Type.Prim(t)
  }
}
def cons(p: Program): Program = {
  val consDecls: Set[String] = p.tds.foldLeft(Set.empty)((consDecls, td) =>
    consDecls ++ td.cons.map(_.name)
  )
  val tconsDecls: Set[String] = p.tds.map(_.name).toSet
  Program(
    p.tds.map(td =>
      TypeDecl(
        td.name,
        td.xs,
        td.cons.map(cb =>
          CBind(cb.name, cb.args.map(t => consType(t, tconsDecls)))
        )
      )
    ),
    p.vds.map(vd => vd.copy(b = consExpr(vd.b, consDecls)))
  )
}

def let_(v: Expr, b: Expr => Expr): Expr = {
  val fresh = freshName()
  Expr.Let(Seq((fresh, v)), b(Expr.Var(fresh)))
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

@main def main(program: String, backend: String, log_path: String): Unit = {
  var x = reify_global_funcs(refresh(cons(drive(CharStreams.fromFileName(s"example/${program}.tv")))))
  println(show(pp(x)))
  x = unnest_match(x)
  println("unnest ok!!!")
  println(show(pp(x)))
  x = cps(simpl(x))
  println("simplification done!!!")
  println(show(pp(x)))
  val tyck = TyckEnv(x)
  // for ((k, v) <- tyck.var_map) {
  //  println((k, pp_type(v)))
  // }
  val cpp_code = codegen(x, backend=backend, log_path=log_path)
  compile(cpp_code)
}
