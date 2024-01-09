package zombie.tvirus.codegen
import zombie.tvirus.parser.*

def reduce_rhs_wildcard(rhs: Seq[(Seq[Pat], Expr)]) = {
  rhs.map((pats, e) => (pats.tail, e))
}

def reduce_rhs_var(name: String, rhs: Seq[(Seq[Pat], Expr)]) = {
  rhs.map((pats, e) =>
    pats.head match {
      case Pat.Wildcard => (pats.tail, e)
      case Pat.Var(x) =>
        (pats.tail, Expr.Let(Seq((x, Expr.Var(name))), e))
    }
  )
}

def reduce_rhs_cons(
    cons_name: String,
    cons_args: Seq[String],
    rhs: Seq[(Seq[Pat], Expr)]
): Seq[(Seq[Pat], Expr)] = {
  rhs.flatMap((pats, e) =>
    pats.head match
      case Pat.Wildcard =>
        Seq((Seq.fill(cons_args.length)(Pat.Wildcard) ++ pats.tail, e))
      case Pat.Cons(name, xs) => {
        if (name == cons_name) {
          Seq((xs ++ pats.tail, e))
        } else {
          Seq()
        }
      }
      case Pat.Var(x) => {
        val let_e = Expr.Let(
          Seq((x, Expr.Cons(cons_name, cons_args.map(Expr.Var)))),
          e
        )
        Seq((cons_args.map(_ => Pat.Wildcard) ++ pats.tail, let_e))
      }
  )
}

class UnnestMatchEnv(p: Program) {
  val typename_to_scons = p.tds
    .map(x => (x.name, x.cons.map(y => SCons(y.name, y.args.length))))
    .toMap

  val consname_to_typename =
    p.tds.flatMap(x => x.cons.map(y => (y.name, x.name))).toMap
}

def transform_program(
    lhs: Seq[Expr],
    rhs: Seq[(Seq[Pat], Expr)],
    env: UnnestMatchEnv
): Expr = {
  assert(rhs.forall((p, _) => p.length == lhs.length))
  if (rhs.length == 0) {
    Expr.Fail()
  } else if (lhs.length == 0) {
    rhs.head(1)
  } else {
    // all possible head pattern
    val pats = rhs.map((pats, _) => pats(0))
    if (
      pats.forall(_ match {
        case Pat.Wildcard => true
        case _            => false
      })
    ) {
      transform_program(lhs.tail, reduce_rhs_wildcard(rhs), env)
    } else if (
      pats.forall(_ match {
        case Pat.Wildcard => true
        case Pat.Var(_)   => true
        case _            => false
      })
    ) {
      val name = freshName()
      Expr.Let(
        Seq((name, lhs.head)),
        transform_program(lhs.tail, reduce_rhs_var(name, rhs), env)
      )
    } else {
      val sconss = env.typename_to_scons(
        env.consname_to_typename(
          pats
            .flatMap(_ match {
              case Pat.Cons(name, _) => Seq(name)
              case _                 => Seq()
            })
            .head
        )
      )
      Expr.Match(
        lhs.head,
        sconss.flatMap(scons => {
          val names: Seq[String] = Seq.fill(scons.narg)({ freshName() })
          val cons = Pat.Cons(scons.name, names.map(name => Pat.Var(name)))
          val reduced_rhs: Seq[(Seq[Pat], Expr)] =
            reduce_rhs_cons(scons.name, names, rhs)
          Seq(
            (
              cons,
              transform_program(
                names.map(Expr.Var) ++ lhs.tail,
                reduced_rhs,
                env
              )
            )
          )
        })
      )
    }
  }
}

def unnest_matching(
    x: Expr,
    cases: Seq[(Pat, Expr)],
    env: UnnestMatchEnv
): Expr = {
  transform_program(Seq(x), cases.map((p, e) => (Seq(p), e)), env)
}

def unnest_match_expr(x: Expr, env: UnnestMatchEnv): Expr = {
  val recurse = x => unnest_match_expr(x, env)
  x match {
    case Expr.Var(v)    => Expr.Var(v)
    case Expr.Abs(x, b) => Expr.Abs(x, recurse(b))
    case Expr.Match(x, cases) =>
      unnest_matching(
        recurse(x),
        cases.map((l, r) => (l, recurse(r))),
        env
      )
    case Expr.App(f, x) =>
      Expr.App(recurse(f), x.map(recurse))
    case Expr.Cons(name, x) => Expr.Cons(name, x.map(recurse))
    case Expr.LitInt(x) => Expr.LitInt(x)
    case Expr.If(i, t, e) => Expr.If(recurse(i), recurse(t), recurse(e))
    case Expr.Prim(l, op, r) => Expr.Prim(recurse(l), op, recurse(r))
    case Expr.Let(xs, body) => Expr.Let(
      xs.map((name, expr) => (name, recurse(expr))),
      recurse(body)
    )
  }
}

def unnest_match(p: Program): Program = {
  val env = UnnestMatchEnv(p)
  refresh(
    Program(
      p.tds,
      p.vds.map(vd => ValueDecl(vd.x, unnest_match_expr(vd.b, env)))
    )
  )
}
