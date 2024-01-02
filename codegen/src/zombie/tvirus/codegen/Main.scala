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

def cps_typedecl(x: TypeDecl) = {
  x
}

var count = 0

def freshName() = {
  count = count + 1
  s"base$count"
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
  }
}

def cps_valuedecl(x: ValueDecl) = {
  ValueDecl(x.x, cps_expr_ho(x.b, (y => y)))
}

def cps(p: Program): Program = {
  Program(p.decls.map(_.match
    case x: TypeDecl  => cps_typedecl(x)
    case x: ValueDecl => cps_valuedecl(x)
  ))
}

def consAux(e: Expr, consDecls: Set[String]): Expr = {
  val recur = x => consAux(x, consDecls)
  e match
    case Expr.Prim(left, op, right) =>
      Expr.Prim(consAux(left, consDecls), op, consAux(right, consDecls))
    case Expr.App(f, xs) =>
      f match
        case Expr.Var(name) if consDecls.contains(name) =>
          Expr.Cons(name, xs.map(consAux(_, consDecls)))
        case _ => Expr.App(consAux(f, consDecls), xs.map(recur))
    case Expr.Abs(xs, b) => Expr.Abs(xs, recur(b))
    case Expr.Let(xs, b) => Expr.Let(xs, consAux(b, consDecls))
    case Expr.Match(x, bs) =>
      Expr.Match(
        consAux(x, consDecls),
        bs.map(b => (b(0), consAux(b(1), consDecls)))
      )
    case Expr.Var(n) => Expr.Var(n)
}

def cons(p: Program): Program = {
  val consDecls: Set[String] = p.decls.foldLeft(Set.empty)((consDecls, decl) =>
    decl match {
      case td: TypeDecl  => consDecls ++ td.cons.map(_.name)
      case vd: ValueDecl => consDecls
    }
  )

  Program(p.decls.map(_ match {
    case vd: ValueDecl => vd.copy(b = consAux(vd.b, consDecls))
    case d             => d
  }))
}

def get_cons(): Seq[SCons] = {
  Seq(SCons("Z", 0), SCons("S", 1))
  // Seq(SCons("Zero", 0), SCons("One", 0), SCons("Two", 0), SCons("Plus", 2), SCons("Mult", 2))
}

def let_(v: Expr, b: Expr => Expr): Expr = {
  val fresh = "meow" // freshName()
  Expr.Let(Seq((fresh, v)), b(Expr.Var(fresh)))
}

def refresh(p: Program): Program = {
  p
}

def merge_abs_app_expr(x: Expr): Expr = {
  val recur = x => merge_abs_app_expr(x)
  x match {
    case Expr.Var(_)              => x
    case Expr.Abs(bindings, body) => Expr.Abs(bindings, recur(body))
    case Expr.Match(x, cases) =>
      Expr.Match(x, cases.map((lhs, rhs) => (lhs, recur(rhs))))
    case Expr.App(Expr.Abs(bindings, body), xs) => {
      assert(bindings.length == xs.length)
      Expr.Let(bindings.zip(xs).map((b, x) => (b, recur(x))), recur(body))
    }
    case Expr.App(f, xs) => {
      Expr.App(recur(f), xs.map(recur))
    }
    case Expr.Cons(name, xs) => {
      Expr.Cons(name, xs.map(recur))
    }
    case Expr.Let(bindings, body) => {
      Expr.Let(bindings.map((n, v) => (n, recur(v))), recur(body))
    }
  }
}

def merge_abs_app(p: Program): Program = {
  Program(p.decls.map(_ match {
    case ValueDecl(x, b) => ValueDecl(x, merge_abs_app_expr(b))
    case td: TypeDecl    => td
  }))
}


def simple_pat_to_name(p: Pat): String = {
  p match {
    case Pat.Var(name) => name
  }
}


def vd_is_fresh(v: ValueDecl): Boolean = {
  true
}

def td_is_fresh(t: TypeDecl): Boolean = {
  true
}

def is_fresh(p: Program): Boolean = {
  p.decls.forall(_ match {
    case vd: ValueDecl => vd_is_fresh(vd)
    case td: TypeDecl  => td_is_fresh(td)
  })
}

def let_analysis(x: Expr, var_map: mutable.Map[String, (Expr, Int)]): Unit = {}

def unlet(x: Expr, var_map: mutable.Map[String, (Expr, Int)]): Expr = {
  val recurse = x => unlet(x, var_map)
  x match {
    case Expr.App(f, xs)          => Expr.App(recurse(f), xs.map(recurse))
    case Expr.Abs(bindings, body) => Expr.Abs(bindings, recurse(body))
    case Expr.Match(x, cases) =>
      Expr.Match(recurse(x), cases.map((lhs, rhs) => (lhs, recurse(rhs))))
    case Expr.Var(n) => {
      var_map.get(n) match {
        case None => Expr.Var(n)
        case Some((e, i)) => {
          if (i > 1) {
            Expr.Var(n)
          } else {
            assert(i == 1)
            recurse(e)
          }
        }
      }
    }
    case _ => x
  }
}

def let_simplification(p: Program): Program = {
  assert(is_fresh(p))
  val var_map = mutable.Map[String, (Expr, Int)]()
  p.decls.map(_ match {
    case vd: ValueDecl => let_analysis(vd.b, var_map)
    case td: TypeDecl  => {}
  })
  p.decls.map(_ match {
    case vd: ValueDecl => ValueDecl(vd.x, unlet(vd.b, var_map))
    case td: TypeDecl  => {}
  })
  p
}

@main def main() = {
  var x = cons(drive(CharStreams.fromFileName("example/mod2.tv")))
  println(pp(x))
  x = let_simplification(merge_abs_app(cps(unnest_match(x))))
  println(pp(x))
  compile(codegen(x))
}
