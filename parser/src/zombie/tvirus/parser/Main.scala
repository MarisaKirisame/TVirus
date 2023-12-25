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
    case _ => x.toString()
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

def cps_expr(x: Expr, k: Expr): Expr = {
  x match {
    case Expr.Var(x) => Expr.App(k, Seq(Expr.Var(x)))
    case Expr.Abs(xs, y) => {
      val k_ = freshName
      Expr.App(
        k,
        Seq(Expr.Abs(xs :+ TBind(k_, None), cps_expr(y, Expr.Var(k_))))
      )
    }
    case Expr.Match(x, cases) => {
      cps_expr(
        x,
        hoas(x_ => Expr.Match(x_, cases.map((l, r) => (l, cps_expr(r, k)))))
      )
    }
    case p @ Expr.App(f, x) => {
      // cps_expr(f, hoas(f_ => cps_expr(x, hoas(x_ => Expr.App(f_, x_)))))
      p
    }
  }
}

def cps_valuedecl(x: ValueDecl) = {
  val k = freshName
  ValueDecl(x.x, Expr.Abs(Seq(TBind(k, None)), cps_expr(x.b, Expr.Var(k))))
}

def cps(p: Program): Program = {
  Program(p.decls.map(_.match
    case x: TypeDecl  => cps_typedecl(x)
    case x: ValueDecl => cps_valuedecl(x)
  ))
}

@main def main() = {
  print(pp(cps(drive(CharStreams.fromFileName("example/list.tv")))))
  // pprint.pprintln(drive(CharStreams.fromFileName("example/list.tv")))
}
