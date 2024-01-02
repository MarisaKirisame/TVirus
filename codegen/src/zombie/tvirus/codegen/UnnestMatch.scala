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

def transform_program(lhs: Seq[Expr], rhs: Seq[(Seq[Pat], Expr)]): Expr = {
  assert(rhs.forall((p, _) => p.length == lhs.length))
  if (rhs.length == 0) {
    Expr.Var("fail")
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
      transform_program(lhs.tail, reduce_rhs_wildcard(rhs))
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
        transform_program(lhs.tail, reduce_rhs_var(name, rhs))
      )
    } else {
      val sconss = get_cons()
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
              transform_program(names.map(Expr.Var) ++ lhs.tail, reduced_rhs)
            )
          )
        })
      )
    }
  }
}

def unnest_matching(x: Expr, cases: Seq[(Pat, Expr)]): Expr = {
  transform_program(Seq(x), cases.map((p, e) => (Seq(p), e)))
}

def unnest_match_expr(x: Expr): Expr = {
  x match {
    case Expr.Var(v)    => Expr.Var(v)
    case Expr.Abs(x, b) => Expr.Abs(x, unnest_match_expr(b))
    case Expr.Match(x, cases) =>
      unnest_matching(
        unnest_match_expr(x),
        cases.map((l, r) => (l, unnest_match_expr(r)))
      )
    case Expr.App(f, x) =>
      Expr.App(unnest_match_expr(f), x.map(unnest_match_expr))
    case Expr.Cons(name, x) => Expr.Cons(name, x.map(unnest_match_expr))
  }
}

def unnest_match(p: Program): Program = {
  refresh(Program(p.decls.map(_ match {
    case ValueDecl(x, b) => ValueDecl(x, unnest_match_expr(b))
    case td: TypeDecl    => td
  })))
}