package zombie.tvirus.codegen
import zombie.tvirus.parser.*
import scala.jdk.CollectionConverters.*
import java.util.IdentityHashMap
import collection.mutable
import cats.syntax.binested

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
      case Type.App(f, x)  => recur(f) && x.forall(recur)
      case Type.Func(l, r) => l.forall(recur) && recur(r)
      case Type.TyCons(_)  => true
      case Type.Prim(_) => true
    }
  }
}
def unify(l_raw: Type, r_raw: Type, err_msg: => String): Unit = {
  def recurse(l: Type, r: Type) = unify(l, r, err_msg)
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
          println(err_msg)
          assert(ll.length == rl.length)
        } else {
          ll.zip(rl).map((x, y) => recurse(x, y))
          recurse(lr, rr)
        }
      }
      case (Type.TyCons(l), Type.TyCons(r)) => {
        assert(l == r)
      }
      case (Type.App(lf, lx), Type.App(rf, rx)) => {
        recurse(lf, rf)
        assert(lx.length == rx.length)
        lx.zip(rx).map((l, r) => recurse(l, r))
      }
      case (Type.Prim(l), Type.Prim(r)) => {
        assert(l == r)
      }
      case _ => {
        println(pp_type(l))
        println(pp_type(r))
        assert(false)
      }
  }
}

class TyckEnv(p: Program) {
  val var_map = mutable.Map[String, Type]()
  val expr_map = IdentityHashMap[Expr, Type]().asScala
  val unvisited = mutable.Map[String, ValueDecl](p.vds.map(vd => (vd.x, vd))*)
  p.tds.map(tyck_td(_, this))
  p.vds.map(tyck_vd(_, this))
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
    case Pat.Var(name) => new_binding(name, env)
  }
}

def remap_type(x: Type, map: Map[String, Type]): Type = {
    val recurse = x => remap_type(x, map)
    resolve(x) match {
        case v@Type.Var(_, _) => {
            map.get(v.name) match {
                case None => v
                case Some(t) => t
            }
        }
        case Type.Func(xs, y) => Type.Func(xs.map(recurse), recurse(y))
        case Type.App(f, xs) => Type.App(recurse(f), xs.map(recurse))
        case Type.TyCons(name) => Type.TyCons(name)
        case Type.Prim(x) => Type.Prim(x)
    }
}

def instantiate(x: Type): Type = {
  resolve(x) match {
    case Type.TypeScheme(xs, y) => remap_type(y, xs.map(x => (x, fresh_tv())).toMap)
    case x                      => x
  }
}

def new_binding(name: String, env: TyckEnv) = {
  val tv = fresh_tv()
        env.var_map.put(name, tv)
        tv
}

def tyck_primop(l: Type, op: PrimOp, r: Type): Type = {
  op match {
    case PrimOp.EQ => {
      unify(l, r, "")
      Type.Prim(PrimType.BOOL)
    }
    case PrimOp.LT | PrimOp.GT | PrimOp.GE | PrimOp.LE => {
      unify(l, Type.Prim(PrimType.INT), "")
      unify(r, Type.Prim(PrimType.INT), "")
      Type.Prim(PrimType.BOOL)
    }
    case PrimOp.MINUS | PrimOp.ADD | PrimOp.MUL | PrimOp.DIV | PrimOp.MOD => {
      unify(l, Type.Prim(PrimType.INT), "")
      unify(r, Type.Prim(PrimType.INT), "")
      Type.Prim(PrimType.INT)
    }
  }
}

def tyck_expr(x: Expr, env: TyckEnv): Type = {
  val recurse = y => tyck_expr(y, env)
  val t = x match {
    case Expr.Var(v) => {
      env.var_map.get(v) match {
        case Some(t) => instantiate(t)
        case None => {
          println(s"not in scope: ${v}")
          assert(false)
        }
      }
    }
    case Expr.InlineVar(x) => {
      recurse(Expr.Var(x))
    }
    case Expr.Abs(bindings, body) => {
      Type.Func(bindings.map(new_binding(_, env)), recurse(body))
    }
    case Expr.Match(x, cases) => {
      val x_ty = recurse(x)
      val out_ty = fresh_tv()
      cases.map((lhs, rhs) => {
        unify(x_ty, tyck_pat(lhs, env), "")
        unify(out_ty, recurse(rhs), "")
      })
      out_ty
    }
    case Expr.App(f, xs) => {
      val out_ty = fresh_tv()
      unify(recurse(f), Type.Func(xs.map(recurse), out_ty), {pp_expr(x)})
      out_ty
    }
    case Expr.Cons(f, xs) => {
      recurse(Expr.App(Expr.Var(f), xs))
    }
    case Expr.DeclValue(t) => t
    case Expr.Let(bindings, body) => {
      bindings.map((lhs, rhs) => unify(new_binding(lhs, env), recurse(rhs), ""))
      recurse(body)
    }
    case Expr.LitInt(_) => Type.Prim(PrimType.INT)
    case Expr.LitBool(_) => Type.Prim(PrimType.BOOL)
    case Expr.If(i, t, e) => {
      unify(recurse(i), Type.Prim(PrimType.BOOL), "")
      val out_ty = fresh_tv()
      unify(recurse(t), out_ty, "")
      unify(recurse(e), out_ty, "")
      out_ty
    }
    case Expr.Prim(l, op, r) => {
      tyck_primop(recurse(l), op, recurse(r))
    }
    case Expr.Fail() => {
      fresh_tv()
    }
  }
  env.expr_map.put(x, t)
  t
}

def fresh_tv(): Type = {
  Type.Var(freshName(), None)
}

def free_tv(t: Type): Set[String] = {
  resolve(t) match {
    case Type.Var(x, _)  => Set(x)
    case Type.App(f, xs) => free_tv(f) ++ xs.map(free_tv).foldLeft(Set[String]())((l, r) => l ++ r)
    case Type.TyCons(_)  => Set()
    case Type.Func(xs, y) => xs.map(free_tv).foldLeft(Set[String]())((l, r) => l ++ r) ++ free_tv(y)
    case Type.Prim(_) => Set()
  }
}

def generalize(t: Type) = {
  Type.TypeScheme(free_tv(t).toSeq, t)
}

def tyck_td(td: TypeDecl, env: TyckEnv): Unit = {
  td.cons.map((cb: CBind) =>
    env.var_map.put(
      cb.name,
      generalize(
        Type.Func(
          cb.args,
          if (td.xs.isEmpty) { Type.TyCons(td.name) }
          else {
            Type.App(Type.TyCons(td.name), td.xs.map(t => Type.Var(t, None)))
          }
        )
      )
    )
  )
}

def tyck_vd(vd: ValueDecl, env: TyckEnv): Unit = {
  if (env.unvisited.contains(vd.x)) {
    env.unvisited.remove(vd.x)
    val tv = fresh_tv()
    env.var_map.put(vd.x, tv)
    unify(tyck_expr(vd.b, env), tv, "")
    env.var_map.put(vd.x, generalize(tv))
  }
}

def tyck_program(p: Program) = {
  TyckEnv(p)
}
