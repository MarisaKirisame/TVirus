package zombie.tvirus.codegen

import zombie.tvirus.parser.*
import zombie.tvirus.prettier.{*, given}
import org.antlr.v4.runtime.CharStreams

extension (ds: Iterable[Doc])
  def interleave(sep: Doc = "," <> Doc.Nl): Doc = ds
    .dropRight(1)
    .map(d => d <> sep)
    .toList
    .appendedAll(ds.lastOption)
    .reduceOption(_ <+> _)
    .getOrElse("")

def pp_type(x: Type): Doc = {
  resolve(x) match {
    case Type.Var(name, _) => name
    case Type.App(f, y) =>
      "(" <> pp_type(f) <> "(" <> Doc.Group(
        y.map(pp_type).interleave()
      ) <> ")" <> ")"
    case Type.Func(l, r) =>
      "(" <> Doc.Group(l.map(pp_type).interleave()) <> ")" <> "=>" <> pp_type(r)
    case Type.TyCons(x) => x
    case Type.TypeScheme(xs, y) =>
      "forall " <> Doc.Group(
        xs.map(Doc.Text.apply).interleave(" ")
      ) <> ". " <> pp_type(y)
    case Type.Prim(PrimType.INT)  => "Int"
    case Type.Prim(PrimType.BOOL) => "Bool"
  }
}

def pp_cbind(x: CBind): Doc = {
  x.name <> "(" <> Doc.Group(x.args.map(pp_type).interleave()) <> ")"
}

def pp_typedecl(x: TypeDecl): Doc = {
  "data " <> x.name <> " " <> Doc.Group(
    x.xs.map(Doc.Text.apply).interleave(" ")
  ) <> " = " <> Doc.Group(x.cons.map(pp_cbind).interleave(" |" <> Doc.Nl))
}

def pp_pat(x: Pat): Doc = {
  x match
    case Pat.Wildcard => "_"
    case Pat.Cons(name, args) =>
      name <> "(" <> Doc.Group(args.map(pp_pat).interleave()) <> ")"
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
}

def pp_expr(x: Expr): Doc = {
  x match
    case Expr.Abs(xs, b) =>
      "(" <> "\\" <> Doc.Group(
        xs.map(Doc.Text.apply).interleave()
      ) <> "." <> (" " <|> Doc.Nl) <> pp_expr(b) <> ")"
    case Expr.App(f, y) =>
      "(" <> pp_expr(f) <> "(" <> Doc.Group(y.map(pp_expr).interleave()) <> ")" <> ")"
    case Expr.Var(y)       => y
    case Expr.InlineVar(y) => y
    case Expr.Match(y, cases) =>
      "(" <> "match " <> pp_expr(y) <> " with" <> Doc.Nl <> cases
        .map((p, b) => "| " <> pp_pat(p) <> " -> " <> pp_expr(b) <> Doc.Nl)
        .reduceOption(_ <+> _)
        .getOrElse("") <> ")"
    case Expr.Cons(cons, xs) =>
      "(" <> cons <> "(" <> Doc.Group(xs.map(pp_expr).interleave()) <> ")"
    case Expr.Let(binding, body) =>
      "let " <> Doc.Group(
        binding.map((n, b) => n <> " = " <> pp_expr(b)).interleave()
      ) <> " in" <> (" " <|> Doc.Nl) <> pp_expr(
        body
      )
    case Expr.LitInt(y) => y.toString
    case Expr.Prim(l, op, r) =>
      pp_expr(l) <> " " <> pp_op(op) <> " " <> pp_expr(r)
    case Expr.LitBool(x) =>
      if (x) { "True" }
      else { "False" }
    case Expr.If(i, t, e) =>
      "if " <> pp_expr(i) <> " {" <> (" " <|> Doc.Nl) <> pp_expr(
        t
      ) <> (" " <|> Doc.Nl) <> "}" <> (" " <|> Doc.Nl) <>
        "else" <> (" " <|> Doc.Nl) <> "{" <> (" " <|> Doc.Nl) <>
        pp_expr(e) <> (" " <|> Doc.Nl) <> "}"
    case Expr.Fail() => "fail"
}

def pp_valdecl(x: ValueDecl): Doc = {
  "let " <> x.x <> " =" <> (" " <|> Doc.Nl) <> pp_expr(x.b)
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
