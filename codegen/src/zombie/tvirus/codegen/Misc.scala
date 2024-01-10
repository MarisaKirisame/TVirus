package zombie.tvirus.codegen
import org.antlr.v4.runtime.CharStreams
import java.io.FileWriter
import java.io.IOException
import zombie.tvirus.parser.*
import collection.mutable

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
    case Expr.Let(xs, b) => Expr.Let(xs.map((l, r) => (l, recurse(r))), recurse(b))
    case Expr.Match(x, bs) =>
      Expr.Match(
        recurse(x),
        bs.map(b => (b(0), recurse(b(1))))
      )
    case Expr.Var(n) => Expr.Var(n)
    case Expr.LitInt(x) => Expr.LitInt(x)
    case Expr.If(i, t, e) => Expr.If(recurse(i), recurse(t), recurse(e))
}

def consType(x: Type, decls: Set[String]): Type = {
  val recurse = x => consType(x, decls)
  resolve(x) match {
    case v@Type.Var(_, _) => if (decls.contains(v.name)) { Type.TyCons(v.name) } else { v }    
    case Type.App(f, xs) => Type.App(recurse(f), xs.map(recurse))
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
    case Expr.Var(n) => Expr.Var(remap.get(n).getOrElse(n))
    case Expr.Abs(bindings, body) => {
      val (new_bindings, new_remap) =
        bindings.foldLeft((Seq[String](), remap))((res, next_name) => {
          val fn = freshName()
          assert(res(1).get(next_name).isEmpty)
          (res(0) :+ fn, res(1) + (next_name -> fn))
        })
      Expr.Abs(new_bindings, refresh_expr(body, new_remap))
    }
    case Expr.Let(bindings, body) => {
      val (new_bindings, new_remap) = bindings.foldLeft(
        (Seq[(String, Expr)](), remap)
      )((res, next_binding) => {
        val next_name = next_binding(0)
        val fn = freshName()
        assert(res(1).get(next_name).isEmpty)
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

def merge_abs_app_expr(x: Expr): Expr = {
  val recurse = x => merge_abs_app_expr(x)
  x match {
    case Expr.Var(_)              => x
    case Expr.Abs(bindings, body) => Expr.Abs(bindings, recurse(body))
    case Expr.Match(x, cases) =>
      Expr.Match(x, cases.map((lhs, rhs) => (lhs, recurse(rhs))))
    case Expr.App(Expr.Abs(bindings, body), xs) => {
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
    case Expr.If(i, t, e) => {
      Expr.If(recurse(i), recurse(t), recurse(e))
    }
    case Expr.Prim(l, op, r) => {
      Expr.Prim(recurse(l), op, recurse(r))
    }
    case Expr.Fail() => Expr.Fail()
  }
}

def merge_abs_app(p: Program): Program = {
  Program(p.tds, p.vds.map(vd => ValueDecl(vd.x, merge_abs_app_expr(vd.b))))
}

def expr_is_fresh(x: Expr, seen: mutable.Set[String]): Boolean = {
  val recurse = x => expr_is_fresh(x, seen)
  x match {
    case Expr.Var(_) => true
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
    case Expr.LitInt(_) => true
    case Expr.If(i, t, e) => recurse(i) && recurse(t) && recurse(e)
    case Expr.Prim(l, op, r) => recurse(l) && recurse(r)
    case Expr.Fail() => true
  }
}

def is_fresh(p: Program): Boolean = {
  val seen = mutable.Set[String]()
  p.vds.forall(vd => expr_is_fresh(vd.b, seen))
}

@main def main() = {
  //val program = "example/mod2.tv"
  //val program = "example/list.tv"
  //val program = "example/taba.tv"
  //val program = "example/pascal.tv"
  //val program = "example/boolean.tv"
  val program = "example/rbt.tv"
  //val program = "example/debug.tv"
  var x = refresh(cons(drive(CharStreams.fromFileName(program))))
  println(pp(x))
  x = let_simplification(merge_abs_app(cps(unnest_match(x))))
  println(pp(x))
  val tyck = TyckEnv(x)
  for ((k, v) <- tyck.var_map) {
    println((k, pp_type(v)))
  }
  val cpp_code = codegen(x)
  compile(cpp_code)
}
