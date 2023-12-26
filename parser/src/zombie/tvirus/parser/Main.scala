package zombie.tvirus.parser
import org.antlr.v4.runtime.CharStreams

def bracket(x: String) = {
  "(" + x + ")"
}

def pp_type(x: Type): String = {
  x match {
    case Type.Var(name) => name
    case Type.App(f, x) => bracket(pp_type(f) + " " + pp_type(x))
    case _              => "unknown"
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
    case _                  => x.toString()
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

def cps_exprs(x: Seq[Expr], k: Seq[Expr] => Expr): Expr = {
  x match {
    case Seq()   => k(Seq())
    case x +: xs => cps_expr_k(x, x_ => cps_exprs(xs, xs_ => k(x_ +: xs_)))
  }
}

def cps_abs(x: Expr.Abs): Expr = {
  val k_ = freshName
  Expr.Abs(
    x.xs :+ TBind(k_, None),
    cps_expr_c(x.b, Expr.Var(k_))
  )
}

def cps_expr_k(x: Expr, k: Expr => Expr): Expr = {
  x match {
    case v@Expr.Var(_) => k(v)
    case abs@Expr.Abs(_, _) => k(cps_abs(abs))
    case Expr.Match(x, cases) => {
      cps_expr_k(
        x,
        x_ => Expr.Match(x_, cases.map((l, r) => (l, cps_expr_k(r, k))))
      )
    }
    case Expr.App(f, x) => {
      cps_expr_k(f, f_ => cps_exprs(x, x_ => Expr.App(f_, x_ :+ hoas(k))))
    }
    case Expr.Cons(name, args) =>
      cps_exprs(args, args_ => k(Expr.Cons(name, args_)))
  }
}

def cps_expr_c(x: Expr, k: Expr): Expr = {
  x match {
    case v@Expr.Var(_) => Expr.App(k, Seq(v))
    case abs@Expr.Abs(_, _) => Expr.App(k, Seq(cps_abs(abs)))
    case Expr.Match(x, cases) => cps_expr_k(x, x_ => Expr.Match(x_, cases.map((l, r) => (l, cps_expr_c(r, k)))))
    case Expr.Cons(name, args) => cps_exprs(args, args_ => Expr.App(k, Seq(Expr.Cons(name, args_))))
    case Expr.App(f, x) => cps_expr_k(f, f_ => cps_exprs(x, x_ => Expr.App(f_, x_ :+ k)))
  }
}

def cps_valuedecl(x: ValueDecl) = {
  val k = freshName
  ValueDecl(
    x.x,
    Expr.Abs(
      Seq(TBind(k, None)),
      cps_expr_c(x.b, Expr.Var(k))
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
  e match
    case Expr.Prim(left, op, right) =>
      Expr.Prim(consAux(left, consDecls), op, consAux(right, consDecls))
    case Expr.App(f, xs) =>
      f match
        case Expr.Var(name) if consDecls.contains(name) =>
          Expr.Cons(name, xs.map(consAux(_, consDecls)))
        case _ => Expr.App(consAux(f, consDecls), xs.map(consAux(_, consDecls)))
    case Expr.Abs(xs, b) => Expr.Abs(xs, consAux(b, consDecls))
    case Expr.Let(xs, b) => Expr.Let(xs, consAux(b, consDecls))
    case Expr.Match(x, bs) =>
      Expr.Match(
        consAux(x, consDecls),
        bs.map(b => (b(0), consAux(b(1), consDecls)))
      )
    case _ => e

}

def cons(p: Program): Program = {
  val consDecls: Set[String] = p.decls.foldLeft(Set.empty)((consDecls, decl) =>
    decl match {
      case td: TypeDecl => consDecls ++ td.cons.map(_.name)
      case _            => consDecls
    }
  )

  Program(p.decls.map(_ match {
    case vd: ValueDecl => vd.copy(b = consAux(vd.b, consDecls))
    case d             => d
  }))
}

@main def main() = {
  print(pp(cps(cons(drive(CharStreams.fromFileName("example/list.tv"))))))
  // pprint.pprintln(drive(CharStreams.fromFileName("example/list.tv")))
}
