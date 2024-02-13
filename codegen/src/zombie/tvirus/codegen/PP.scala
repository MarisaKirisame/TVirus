package zombie.tvirus.codegen

import zombie.tvirus.parser.*
import zombie.tvirus.prettier.{Doc, given}
import Doc.<>
import org.antlr.v4.runtime.CharStreams

extension (ds: Iterable[Doc])
  def interleave(
      sep: Doc = "," <> Doc.nl,
      reduceFunc: (Doc, Doc) => Doc = _ <> _
  ): Doc = ds
    .dropRight(1)
    .map(d => d <> sep)
    .toList
    .appendedAll(ds.lastOption)
    .reduceOption(reduceFunc(_, _))
    .getOrElse("")

def pp_type(x: Type): Doc = {
  resolve(x) match {
    case Type.Var(name, None) => name
    case Type.App(f, y) =>
      pp_type(f) <> Doc.bracketed(Doc.group(y.map(pp_type).interleave()))
    case Type.Func(l, r) =>
      Doc.bracketed(
        Doc.group(l.map(pp_type).interleave())
      ) <> " => " <> pp_type(r)
    case Type.TyCons(x) => x
    case Type.TypeScheme(xs, y) =>
      "forall " <> Doc.group(
        xs.map(Doc.text).interleave(" ")
      ) <> ". " <> pp_type(y)
    case Type.Prim(PrimType.INT)    => "Int"
    case Type.Prim(PrimType.BOOL)   => "Bool"
    case Type.Prim(PrimType.STRING) => "String"
  }
}

def pp_cbind(x: CBind): Doc =
  x.name <> Doc.bracketed(Doc.group(x.args.map(pp_type).interleave()))

def pp_typedecl(x: TypeDecl): Doc = {
  "data " <> x.name <> " " <> Doc.group(
    x.xs.map(Doc.text).interleave(" ")
  ) <> " = " <> Doc.group(
    Doc.nest(2, x.cons.map(pp_cbind).interleave(" |" <> Doc.nl))
  )
}

def pp_pat(x: Pat): Doc = {
  x match
    case Pat.Wildcard => "_"
    case Pat.Cons(name, args) =>
      name <> Doc.bracketed(Doc.group(args.map(pp_pat).interleave()))
    case Pat.Var(x) => x
}

def pp_op(op: PrimOp): Doc = {
  op match
    case PrimOp.EQ    => "=="
    case PrimOp.ADD   => "+"
    case PrimOp.MUL   => "*"
    case PrimOp.MINUS => "-"
    case PrimOp.LT    => "<"
    case PrimOp.GT    => ">"
    case PrimOp.DIV   => "/"
    case PrimOp.MOD   => "%"
    case PrimOp.LE    => "<="
    case PrimOp.GE    => ">="
}

def pp_expr(x: Expr): Doc = {
  x match
    case Expr.Abs(xs, b) =>
      Doc.bracketed(
        "\\" <> Doc.group(
          xs.map(Doc.text).interleave()
        ) <> "." <> Doc.nest(2, Doc.sbreak <> pp_expr(b))
      )
    case Expr.App(f, y) =>
      pp_expr(f) <> Doc.bracketed(
        Doc.group(
          y.map(pp_expr).interleave()
        )
      )

    case Expr.Var(y)       => y
    case Expr.GVar(y)       => y
    case Expr.InlineVar(y) => y
    case Expr.Match(y, cases) =>
      Doc.bracketed(
        "match " <> pp_expr(y) <> " with" <> Doc.nest(
          2,
          Doc.nl <>
            cases
              .map((p, b) =>
                "| " <> pp_pat(p) <> " -> " <> Doc.nest(2, pp_expr(b))
              )
              .interleave(Doc.nl)
        )
      )
    case Expr.Cons(cons, xs) =>
      Doc.bracketed(
        cons <> Doc.bracketed(Doc.group(xs.map(pp_expr).interleave()))
      )
    case Expr.Let(binding, body) => {
      val bindings =
        Doc.group(binding.map((n, b) => n <> " = " <> pp_expr(b)).interleave())
      val header = ("let " <> bindings <> " in") <|> ("let " <> Doc.nest(
        2,
        Doc.nl <> bindings
      ) <> Doc.nl <> "in")
      header <> Doc.sbreak <> pp_expr(body)
    }

    case Expr.LitInt(y) => y.toString

    case Expr.Prim(l, op, r) =>
      pp_expr(l) <> " " <> pp_op(op) <> " " <> pp_expr(r)

    case Expr.PrimCPS(l, op, r, k) =>
      "cont_" <> pp_expr(k) <> Doc.bracketed(
        pp_expr(l) <> " " <> pp_op(op) <> " " <> pp_expr(r)
      )

    case Expr.LitBool(x) =>
      if (x) { "True" }
      else { "False" }
    case Expr.If(i, t, e) => {
      val cond = pp_expr(i)
      val bt = pp_expr(t)
      val bf = pp_expr(e)
      ("if " <> cond <> " { " <> Doc.flatten(bt) <> " } else { " <> Doc.flatten(
        bf
      ) <> " }") <|>
        ("if " <> cond <> " {" <> Doc.nest(
          2,
          Doc.nl <> bt
        ) <> Doc.nl <> "} else {" <> Doc.nest(2, Doc.nl <> bf) <> Doc.nl <> "}")
    }
    case Expr.Fail() => "fail"
    case Expr.DeclValue(t) => "?:" <> pp_type(t)
}

def pp_valdecl(x: ValueDecl): Doc = {
  "let " <> x.x <> " =" <> Doc.sbreak <> pp_expr(x.b)
}

def pp(x: Program): Doc = {
  x.tds.map(pp_typedecl).interleave(Doc.nl) <> Doc.nl <>
    x.vds.map(pp_valdecl).interleave(Doc.nl)
}

def show(x: Doc): String = {
  x.printed()
}

extension [A, B](a: A) infix def |>(f: A => B): B = f(a)

// @main
// def testShow() = {
//   val s = CharStreams.fromFileName("example/rbt.tv")
//     |> drive
//     |> cons
//     |> refresh
//     |> simpl
//     |> unnest_match
//     |> simpl
//     |> pp
//     |> show
//   println(s)
// }
