package zombie.tvirus.codegen

import zombie.tvirus.parser.*

def member[T](x: T, s: Seq[T]): Boolean = {
  s.map(y => x == y).reduce((a, b) => (a || b))
}

def function_type_p(t: Type): Option[Seq[String]] = {
  t match {
    case Type.Prim(pt) => None
    case Type.Func(xs, r) => Some(xs.map(_ => freshName()))
    case Type.Var(name, Some(ty)) => function_type_p(ty)
    case Type.Var(name, None) => None
    case Type.App(f, xs) => function_type_p(f)
    case Type.TyCons(name) => None
    case Type.TypeScheme(xs, t) => function_type_p(t)
  }
}

def reify_global_funcs_aux(e: Expr, g: Seq[String], t: TyckEnv): Expr = {
  val rec = (e) => reify_global_funcs_aux(e, g, t)
  e match {
    case Expr.Prim(l, op, r) =>
      Expr.Prim(rec(l), op, rec(r))
    case Expr.Var(name) =>
      if (member(name, g)) {
        function_type_p(t.var_map(name)) match {
          case Some(formals) => {
            Expr.Abs(
              formals,
              Expr.App(Expr.Var(name), formals.map(formal => Expr.Var(formal)))
            )
          }
          case None => e
        }
      } else e
    case Expr.LitInt(x)  => e
    case Expr.LitBool(x) => e
    case Expr.App(f, xs) => Expr.App(rec(f), xs.map(rec))
    case Expr.Abs(xs, b) => Expr.Abs(xs, rec(b))
    case Expr.Let(xs, b) =>
      Expr.Let(
        xs.map(binding =>
          binding match {
            case (name, value) => (name, rec(value))
          }
        ),
        rec(b)
      )
    case Expr.Match(x, bs) =>
      Expr.Match(
        rec(x),
        bs.map(pattern =>
          pattern match {
            case (pat, expr) => (pat, rec(expr))
          }
        )
      )
    case Expr.Cons(name, args)      => Expr.Cons(name, args.map(rec))
    case Expr.DeclValue(t)          => e
    case Expr.If(cond, conseq, alt) => Expr.If(rec(cond), rec(conseq), rec(alt))
    case _                          => Expr.Fail()
  }
}

def reify_global_funcs(prog: Program): Program = {
  val tds = prog.tds
  val vds = prog.vds
  val globals = vds.map(vd => vd.x).filter(name => name != "main")
  val tyck = tyck_program(prog)
  Program(
    tds,
    vds.map(vd => ValueDecl(vd.x, reify_global_funcs_aux(vd.b, globals, tyck)))
  )
}
