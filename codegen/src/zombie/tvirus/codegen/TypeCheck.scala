package  zombie.tvirus.codegen
import zombie.tvirus.parser.* 
import scala.jdk.CollectionConverters.*
import java.util.IdentityHashMap
import collection.mutable

def resolve(t: Type): Type = {
  t match
    case t @ Type.Var(_, None) => t
    case t @ Type.Var(_, Some(x)) => {
      val r = resolve(x)
      t.ty = Some(r)
      r
    }
    case t => t
}

def occur_check(l: Type, r: Type): Boolean = {
  val recur = (t: Type) => occur_check(l, resolve(t))
  if (l eq r) {
    false
  } else {
    r match {
      case Type.Var(_, _)  => true
      case Type.App(f, x)  => recur(f) && recur(x)
      case Type.Func(l, r) => l.forall(recur) && recur(r)
      case Type.TyCons(_)  => true
    }
  }
}
def unify(l_raw: Type, r_raw: Type): Unit = {
  val l = resolve(l_raw)
  val r = resolve(r_raw)
  if (l ne r) {
    if (is_var(l) && !occur_check(l, r)) {
      println(pp_type(l))
      println(pp_type(r))
      assert(false)
    }
    if (is_var(r) && !occur_check(r, l)) {
      println(l)
      println(pp_type(l))
      println(pp_type(r))
      assert(false)
    }
    (l, r) match
      case (l @ Type.Var(_, None), r) => l.ty = Some(r)
      case (_, r @ Type.Var(_, None)) => r.ty = Some(l)
      case (Type.Func(ll, lr), Type.Func(rl, rr)) => {
        if (ll.length != rl.length) {
          println(pp_type(l))
          println(pp_type(r))
          assert(ll.length == rl.length)
        } else {
          ll.zip(rl).map((x, y) => unify(x, y))
          unify(lr, rr)
        }
      }
      case (Type.TyCons(l), Type.TyCons(r)) => {
        assert(l == r)
      }
      case _ => {
        print(pp_type(l))
        print(pp_type(r))
        assert(false)
      }
  }
}

class TyckEnv {
  val var_map = mutable.Map[String, Type]()
  val expr_map = IdentityHashMap[Expr, Type]().asScala
}

def tyck_pat(x: Pat, env: TyckEnv): Type = {
  val recur = y => tyck_pat(y, env)
  x match {
    case Pat.Wildcard => fresh_tv()
    case Pat.Cons(name, xs) =>
      tyck_expr(
        Expr.App(Expr.Var(name), xs.map(x => Expr.DeclValue(recur(x)))),
        env
      )
    case Pat.Var(name) => tyck_expr(Expr.Var(name), env)
  }
}

def tyck_expr(x: Expr, env: TyckEnv): Type = {
  val recur = y => tyck_expr(y, env)
  val t = x match {
    case Expr.Var(v) => {
      env.var_map.get(v) match {
        case Some(t) => t
        case None => {
          val tv = fresh_tv()
          env.var_map.put(v, tv)
          tv
        }
      }
    }
    case Expr.Abs(bindings, body) => {
      Type.Func(bindings.map(bind => recur(Expr.Var(bind))), recur(body))
    }
    case Expr.Match(x, cases) => {
      val x_ty = recur(x)
      val out_ty = fresh_tv()
      cases.map((lhs, rhs) => {
        unify(x_ty, tyck_pat(lhs, env))
        unify(out_ty, recur(rhs))
      })
      out_ty
    }
    case Expr.App(f, xs) => {
      val out_ty = fresh_tv()
      unify(recur(f), Type.Func(xs.map(recur), out_ty))
      out_ty
    }
    case Expr.Cons(f, xs) => {
      recur(Expr.App(Expr.Var(f), xs))
    }
    case Expr.DeclValue(t) => t
    case Expr.Let(bindings, body) => {
      bindings.map((lhs, rhs) => unify(recur(Expr.Var(lhs)), recur(rhs)))
      recur(body)
    }
  }
  env.expr_map.put(x, t)
  t
}

def fresh_tv(): Type = {
  Type.Var(freshName(), None)
}

def TyApps(f: Type, xs: Seq[Type]): Type = {
  if (xs.isEmpty) {
    f
  } else {
    TyApps(Type.App(f, xs.head), xs.tail)
  }
}

def tyck_td(td: TypeDecl, env: TyckEnv): Unit = {
  td.cons.map((cb: CBind) =>
    env.var_map.put(
      cb.name,
      Type.Func(
        cb.args,
        TyApps(Type.TyCons(td.name), td.xs.map(t => Type.Var(t, None)))
      )
    )
  )
}

def tyck_vd(vd: ValueDecl, env: TyckEnv): Unit = {
  env.var_map.put(vd.x, tyck_expr(vd.b, env))
}

def tyck_program(p: Program) = {
  val env = TyckEnv()
  p.decls.map(_ match {
    case vd: ValueDecl => {}
    case td: TypeDecl  => tyck_td(td, env)
  })
  p.decls.map(_ match {
    case vd: ValueDecl => tyck_vd(vd, env)
    case td: TypeDecl  => {}
  })
  env
}