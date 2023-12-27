package zombie.tvirus.parser
import org.antlr.v4.runtime.CharStreams

def bracket(x: String) = {
  "(" + x + ")"
}

def pp_type(x: Type): String = {
  x match {
    case Type.Var(name) => name
    case Type.App(f, x) => bracket(pp_type(f) + " " + pp_type(x))
  }
}

def pp_cbind(x: CBind) = {
  x.name + " " + x.args.map(pp_type).mkString(" ")
}

def pp_typedecl(x: TypeDecl) = {
  x.name + " " + x.xs
    .mkString(",") + " = " + x.cons.map(pp_cbind).mkString(" | ")
}

def pp_pat(x: Pat): String =
  x match
    case Pat.Wildcard         => "_"
    case Pat.Cons(name, args) => s"$name " + args.map(pp_pat(_)).mkString(" ")
    case Pat.Var(x)           => x

def hoas(f: (Expr => Expr)): Expr = {
  val x_ = freshName
  Expr.Abs(Seq(TBind(x_, None)), f(Expr.Var(x_)))
}

def pp_binding(bind: (SBind, Expr)): String = {
  bind(0).name + " = " + pp_expr(bind(1))
}

def pp_expr(x: Expr): String = {
  x match {
    case Expr.Abs(xs, b) =>
      bracket("\\" + xs.map(_.name).mkString(" ") + " -> " + pp_expr(b))
    case Expr.App(f, x) =>
      bracket(pp_expr(f) + "(" + x.map(pp_expr).mkString(", ") + ")")
    case Expr.Var(x) => x
    case Expr.Match(x, cases) =>
      "match " + pp_expr(x) + " with " + cases
        .map(y => pp_pat(y(0)) + " => " + pp_expr(y(1)))
        .mkString(" | ")
    case Expr.Cons(con, xs) => con + "(" + xs.map(pp_expr).mkString(", ") + ")"
    case Expr.Let(binding, body) => "let " + binding.map(pp_binding).mkString(", ") + " in " + pp_expr(body)
  }
}

def pp_valdecl(x: ValueDecl) = {
  x.x.name + " = " + pp_expr(x.b)
}

def pp(x: Program) = {
  x.decls
    .map(y =>
      y match {
        case t: TypeDecl  => pp_typedecl(t)
        case v: ValueDecl => pp_valdecl(v)
      }
    )
    .mkString("\n") + "\n"
}

def cps_typedecl(x: TypeDecl) = {
  x
}

var count = 0

def freshName = {
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
      case FO(k) => (x:Expr) => Expr.App(k, Seq(x))
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
    case v@Expr.Var(_) => k.toHO(v)
    case Expr.Abs(xs, b) => {
      val k_ = freshName
      k.toHO(Expr.Abs(
        xs :+ TBind(k_, None),
        cps_expr_fo(b, Expr.Var(k_))
      ))
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
    case Expr.Cons(name, args) =>
      cps_exprs(args, args_ => k.toHO(Expr.Cons(name, args_)))
  }
}

def cps_valuedecl(x: ValueDecl) = {
  val k = freshName
  ValueDecl(
    x.x,
    Expr.Abs(
      Seq(TBind(k, None)),
      cps_expr_fo(x.b, Expr.Var(k))
    )
  )
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
}

def let_(v: Expr, b: Expr => Expr): Expr = {
  val fresh = freshName
  Expr.Let(Seq((SBind(fresh, None), v)), b(Expr.Var(fresh)))
}

def reduce_rhs_wildcard(rhs: Seq[(Seq[Pat], Expr)]) = {
  rhs.map((pats, e) => (pats.tail, e))
}

def reduce_rhs_var(name: String, rhs: Seq[(Seq[Pat], Expr)]) = {
  rhs.map((pats, e) => pats.head match {
    case Pat.Wildcard => (pats.tail, e)
    case Pat.Var(x) => (pats.tail, Expr.Let(Seq((SBind(x, None), Expr.Var(name))), e))
  })
}

def reduce_rhs_cons(cons_name: String, cons_args: Seq[String], rhs: Seq[(Seq[Pat], Expr)]): Seq[(Seq[Pat], Expr)] = {
  rhs.flatMap((pats, e) => pats.head match
    case Pat.Wildcard => Seq((Seq.fill(cons_args.length)(Pat.Wildcard) ++ pats.tail, e))
    case Pat.Cons(name, xs) => {
      if (name == cons_name) {
        Seq((xs ++ pats.tail, e))
      } else {
        Seq()        
      }
    }
    case Pat.Var(x) => {
      val let_e = Expr.Let(Seq((SBind(x, None), Expr.Cons(cons_name, cons_args.map(Expr.Var)))), e)
      Seq((cons_args.map(_ => Pat.Wildcard) ++ pats.tail, let_e))
    }
  )
}

def transform_program(lhs: Seq[Expr], rhs: Seq[(Seq[Pat], Expr)]): Expr = {
  assert(rhs.forall((p, _) => p.length == lhs.length))
  if (rhs.length == 0) {
    Expr.Var("fail")
  }
  else if (lhs.length == 0) {
    rhs.head(1)
  } else {
    // all possible head pattern
    val pats = rhs.map((pats, _) => pats(0))
    if (pats.forall(_ match { case Pat.Wildcard => true case _ => false })) {
      transform_program(lhs.tail, reduce_rhs_wildcard(rhs))
    } else if (pats.forall(_ match { case Pat.Wildcard => true case Pat.Var(_) => true case _ => false })) {
      val name = freshName
      Expr.Let(Seq((SBind(name, None), lhs.head)), transform_program(lhs.tail, reduce_rhs_var(name, rhs)))
    } else {
      val sconss = get_cons()
      Expr.Match(lhs.head, sconss.flatMap(scons => {
        val names:Seq[String] = Seq.fill(scons.narg)(freshName)
        val cons = Pat.Cons(scons.name, names.map(name => Pat.Var(name)))
        val reduced_rhs: Seq[(Seq[Pat], Expr)] = reduce_rhs_cons(scons.name, names, rhs)
        Seq((cons, transform_program(names.map(Expr.Var) ++ lhs.tail, reduced_rhs)))
      }))
    }
  }
}

def unnest_matching(x: Expr, cases: Seq[(Pat, Expr)]): Expr = {
  transform_program(Seq(x), cases.map((p, e) => (Seq(p), e)))
}

def unnest_match_expr(x: Expr): Expr = {
  x match {
    case Expr.Var(v) => Expr.Var(v)
    case Expr.Abs(x, b) => Expr.Abs(x, unnest_match_expr(b))
    case Expr.Match(x, cases) => unnest_matching(unnest_match_expr(x), cases.map((l, r) => (l, unnest_match_expr(r))))
    case Expr.App(f, x) => Expr.App(unnest_match_expr(f), x.map(unnest_match_expr))
    case Expr.Cons(name, x) => Expr.Cons(name, x.map(unnest_match_expr))
  }
}

def unnest_match(p: Program): Program = {
  Program(p.decls.map(_ match {
    case ValueDecl(x, b) => ValueDecl(x, unnest_match_expr(b))
    case td: TypeDecl => td
  }))
}

@main def main() = {
  val x = cons(drive(CharStreams.fromFileName("example/mod2.tv")))
  print(pp(x))
  print(pp(unnest_match(x)))
  // pprint.pprintln(drive(CharStreams.fromFileName("example/list.tv")))
}
