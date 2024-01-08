package zombie.tvirus.parser

import scala.jdk.CollectionConverters.*

import zombie.tvirus.parser.generated.*;
import TVirusParser.*;
import org.antlr.v4.runtime.tree.TerminalNode;
import org.antlr.v4.runtime.CharStream
import org.antlr.v4.runtime.CommonTokenStream

object TVirusParserPrimOpVisitor extends TVirusParserBaseVisitor[PrimOp] {
  import TVirusLexer.*
  import PrimOp.*

  override def visitPrimOp(ctx: PrimOpContext): PrimOp =
    ctx.getChild(0).asInstanceOf[TerminalNode].getSymbol().getType() match
      case SYM_ADD   => ADD
      case SYM_MINUS => MINUS
      case SYM_MUL   => MUL
      case SYM_DIV   => DIV
      case SYM_DE    => EQ
      case SYM_NE    => NE
      case SYM_GT    => GT
      case SYM_LT    => LT
      case SYM_GE    => GE
      case SYM_LE    => LE
}

object TVirusParserPrimTypeVisitor extends TVirusParserBaseVisitor[PrimType] {
  import TVirusLexer.*
  import PrimType.*

  override def visitPrimType(ctx: PrimTypeContext): PrimType =
    ctx.getChild(0).asInstanceOf[TerminalNode].getSymbol().getType() match
      case KW_INT  => INT
      case KW_BOOL => BOOL
}

object TVirusParserTypeVisitor extends TVirusParserBaseVisitor[Type] {
  override def visitTypePrim(ctx: TypePrimContext): Type =
    Type.Prim(TVirusParserPrimTypeVisitor.visit(ctx.primType()))

  override def visitTypeParen(ctx: TypeParenContext): Type =
    visit(ctx.`type`())

  override def visitTypeFunc(ctx: TypeFuncContext): Type =
    Type.Func(Seq(visit(ctx.`type`(0))), visit(ctx.`type`(1)))

  override def visitTypeVar(ctx: TypeVarContext): Type =
    Type.Var(ctx.IDENT().getSymbol().getText(), None)

  override def visitTypeApp(ctx: TypeAppContext): Type =
    Type.App(visit(ctx.`type`(0)), Seq(visit(ctx.`type`(1))))
}

object TVirusParserSchemeVisitor extends TVirusParserBaseVisitor[Scheme] {
  override def visitSchemePoly(ctx: SchemePolyContext): Scheme =
    Scheme.Poly(
      ctx.IDENT().asScala.toSeq.map(_.getSymbol().getText()),
      TVirusParserTypeVisitor.visit(ctx.`type`())
    )
}

object TVirusParserTBindVisitor extends TVirusParserBaseVisitor[String] {
  override def visitTBind(ctx: TBindContext): String =
    ctx.IDENT().getSymbol().getText()

}

object TVirusParserSBindVisitor extends TVirusParserBaseVisitor[String] {
  override def visitSBind(ctx: SBindContext): String =
    ctx.IDENT().getSymbol().getText()
}

object TVirusParserCBindVisitor extends TVirusParserBaseVisitor[CBind] {
  override def visitCBindEmpty(ctx: CBindEmptyContext): CBind =
    CBind(ctx.IDENT().getSymbol().getText(), Seq.empty)

  override def visitCBindFull(ctx: CBindFullContext): CBind =
    CBind(
      ctx.IDENT().getSymbol().getText(),
      ctx.`type`().asScala.map(TVirusParserTypeVisitor.visit).toSeq
    )
}

object TVirusParserTypeDeclVisitor extends TVirusParserBaseVisitor[TypeDecl] {
  override def visitTypeDecl(ctx: TypeDeclContext): TypeDecl =
    TypeDecl(
      ctx.IDENT(0).getSymbol().getText(),
      ctx.IDENT().asScala.toSeq.drop(1).map(_.getSymbol().getText()),
      ctx.cBind().asScala.toSeq.map(TVirusParserCBindVisitor.visit)
    )
}

object TVirusParserPatVisitor extends TVirusParserBaseVisitor[Pat] {
  override def visitPatWildcard(ctx: PatWildcardContext): Pat =
    Pat.Wildcard

  override def visitPatVar(ctx: PatVarContext): Pat =
    Pat.Var(ctx.IDENT().getSymbol().getText())

  override def visitPatConsEmpty(ctx: PatConsEmptyContext): Pat =
    Pat.Cons(
      ctx.IDENT().getSymbol().getText(),
      Seq.empty
    )

  override def visitPatConsFull(ctx: PatConsFullContext): Pat =
    Pat.Cons(
      ctx.IDENT().getSymbol().getText(),
      ctx.pat().asScala.toSeq.map(visit)
    )
}

object TVirusParserExprVisitor extends TVirusParserBaseVisitor[Expr] {
  override def visitExprPrimOp(ctx: ExprPrimOpContext): Expr =
    Expr.Prim(
      visit(ctx.expr(0)),
      TVirusParserPrimOpVisitor.visit(ctx.primOp()),
      visit(ctx.expr(1))
    )

  override def visitExprVar(ctx: ExprVarContext): Expr =
    Expr.Var(ctx.IDENT().getSymbol().getText())

  override def visitExprParen(ctx: ExprParenContext): Expr =
    visit(ctx.expr())

  override def visitExprLitInt(ctx: ExprLitIntContext): Expr =
    Expr.LitInt(Integer.parseInt(ctx.LIT_INT().getSymbol().getText()))

  override def visitExprAppEmpty(ctx: ExprAppEmptyContext): Expr =
    Expr.App(visit(ctx.expr()), Seq.empty)

  override def visitExprAppFull(ctx: ExprAppFullContext): Expr =
    Expr.App(visit(ctx.expr(0)), ctx.expr().asScala.drop(1).map(visit).toSeq)

  override def visitExprAbs(ctx: ExprAbsContext): Expr =
    Expr.Abs(
      ctx.tBind().asScala.toSeq.map(TVirusParserTBindVisitor.visit),
      visit(ctx.expr())
    )

  override def visitExprLet(ctx: ExprLetContext): Expr =
    Expr.Let(
      ctx
        .sBind()
        .asScala
        .map(TVirusParserSBindVisitor.visit)
        .zip(ctx.expr().asScala.map(visit))
        .toSeq,
      visit(ctx.expr().get(ctx.expr().size() - 1))
    )

  override def visitExprMatch(ctx: ExprMatchContext): Expr =
    Expr.Match(
      visit(ctx.expr(0)),
      ctx
        .pat()
        .asScala
        .map(TVirusParserPatVisitor.visit)
        .zip(ctx.expr().asScala.drop(1).map(visit))
        .toSeq
    )

  override def visitExprIf(ctx: ExprIfContext): Expr = {
    val exprs = ctx.expr().asScala.map(TVirusParserExprVisitor.visit).toSeq
    Expr.If(exprs(0), exprs(1), exprs(2))
  }
}

object TVirusParserValueDeclVisitor extends TVirusParserBaseVisitor[ValueDecl] {
  override def visitValueDecl(ctx: ValueDeclContext): ValueDecl =
    ValueDecl(
      TVirusParserSBindVisitor.visit(ctx.sBind()),
      TVirusParserExprVisitor.visit(ctx.expr())
    )
}

object TVirusParserProgramVisitor extends TVirusParserBaseVisitor[Program] {
  override def visitProgram(ctx: ProgramContext): Program = {
    Program(
      ctx.children.asScala.toSeq.flatMap(t =>
        if (t.isInstanceOf[TypeDeclContext]) {
          Seq(
            TVirusParserTypeDeclVisitor.visit(t.asInstanceOf[TypeDeclContext])
          )
        } else if (t.isInstanceOf[ValueDeclContext]) {
          Seq()
        } else {
          assert(false)
        }
      ),
      ctx.children.asScala.toSeq.flatMap(t =>
        if (t.isInstanceOf[TypeDeclContext]) {
          Seq()
        } else if (t.isInstanceOf[ValueDeclContext]) {
          Seq(
            TVirusParserValueDeclVisitor.visit(t.asInstanceOf[ValueDeclContext])
          )
        } else {
          assert(false)
        }
      )
    )
  }
}

def drive(source: CharStream) = {
  val lexer = TVirusLexer(source)
  val stream = CommonTokenStream(lexer)
  val parser = TVirusParser(stream)

  TVirusParserProgramVisitor.visit(parser.program())
}
