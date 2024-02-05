package zombie.tvirus.codegen

import zombie.tvirus.parser.*
import zombie.tvirus.prettier.{*, given}
import org.antlr.v4.runtime.CharStreams

extension (ds: Iterable[Doc])
  def interleave(
      sep: Doc = "," <> Doc.Nl,
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
    case Type.Var(name, _) => name
    case Type.App(f, y) =>
      pp_type(f) <> Doc.bracketed(Doc.Group(y.map(pp_type).interleave()))
    case Type.Func(l, r) =>
      Doc.bracketed(
        Doc.Group(l.map(pp_type).interleave())
      ) <> " => " <> pp_type(r)
    case Type.TyCons(x) => x
    case Type.TypeScheme(xs, y) =>
      "forall " <> Doc.Group(
        xs.map(Doc.Text.apply).interleave(" ")
      ) <> ". " <> pp_type(y)
    case Type.Prim(PrimType.INT)    => "Int"
    case Type.Prim(PrimType.BOOL)   => "Bool"
    case Type.Prim(PrimType.STRING) => "String"
  }
}

def pp_cbind(x: CBind): Doc =
  x.name <> Doc.bracketed(Doc.Group(x.args.map(pp_type).interleave()))

def pp_typedecl(x: TypeDecl): Doc = {
  "data " <> x.name <> " " <> Doc.Group(
    x.xs.map(Doc.Text.apply).interleave(" ")
  ) <> " = " <> Doc.Group(
    Doc.Nest(2, x.cons.map(pp_cbind).interleave(" |" <> Doc.Nl))
  )
}

def pp_pat(x: Pat): Doc = {
  x match
    case Pat.Wildcard => "_"
    case Pat.Cons(name, args) =>
      name <> Doc.bracketed(Doc.Group(args.map(pp_pat).interleave()))
    case Pat.Var(x) => x
}

def pp_op(op: PrimOp): Doc = {
  op match
    case PrimOp.EQ    => "=="
    case PrimOp.ADD   => "+"
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
        "\\" <> Doc.Group(
          xs.map(Doc.Text.apply).interleave()
        ) <> "." <> Doc.Nest(2, Doc.SBreak <> pp_expr(b))
      )
    case Expr.App(f, y) =>
      pp_expr(f) <> Doc.bracketed(
        Doc.Group(
          y.map(pp_expr).interleave()
        )
      )

    case Expr.Var(y)       => y
    case Expr.GVar(y)       => y
    case Expr.InlineVar(y) => y
    case Expr.Match(y, cases) =>
      Doc.bracketed(
        "match " <> pp_expr(y) <> " with" <> Doc.Nest(
          2,
          Doc.Nl <>
            cases
              .map((p, b) => "| " <> pp_pat(p) <> " -> " <> Doc.Nest(2, pp_expr(b)))
              .interleave(Doc.Nl)
        )
      )
    case Expr.Cons(cons, xs) =>
      Doc.bracketed(
        cons <> Doc.bracketed(Doc.Group(xs.map(pp_expr).interleave()))
      )
    case Expr.Let(binding, body) => {
      val bindings =
        Doc.Group(binding.map((n, b) => n <> " = " <> pp_expr(b)).interleave())
      val header = ("let " <> bindings <> " in") <|> ("let " <> Doc.Nest(
        2,
        Doc.Nl <> bindings
      ) <> Doc.Nl <> "in")
      header <> Doc.SBreak <> pp_expr(body)
    }

    case Expr.LitInt(y) => y.toString

    case Expr.Prim(l, op, r) =>
      pp_expr(l) <> " " <> pp_op(op) <> " " <> pp_expr(r)

    case Expr.PrimCPS(l, op, r, k) =>
      "cont_" <> pp_expr(k) <> Doc.bracketed(pp_expr(l) <> " " <> pp_op(op) <> " " <> pp_expr(r))

    case Expr.LitBool(x) =>
      if (x) { "True" }
      else { "False" }
    case Expr.If(i, t, e) => {
      val cond = pp_expr(i)
      val bt = pp_expr(t)
      val bf = pp_expr(e)
      ("if " <> cond <> " { " <> Doc.Flatten(bt) <> " } else { " <> Doc.Flatten(bf) <> " }") <|>
        ("if " <> cond <> " {" <> Doc.Nest(
          2,
          Doc.Nl <> bt
        ) <> Doc.Nl <> "} else {" <> Doc.Nest(2, Doc.Nl <> bf) <> Doc.Nl <> "}")
    }
    case Expr.Fail() => "fail"
}

def pp_valdecl(x: ValueDecl): Doc = {
  "let " <> x.x <> " =" <> Doc.SBreak <> pp_expr(x.b)
}

def pp(x: Program): Doc = {
  x.tds.map(pp_typedecl).interleave(Doc.Nl) <> Doc.Nl <>
    x.vds.map(pp_valdecl).interleave(Doc.Nl)
}

def show(x: Doc): String = {
  x.resolved(using Cost.sumOfSquared(80))
}

def testShow(x: String): String = {
  show(pp(drive(CharStreams.fromFileName(x))))
}
