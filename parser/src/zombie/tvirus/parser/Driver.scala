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
      case KW_INT => INT
}

object TVirusParserTypeVisitor extends TVirusParserBaseVisitor[Type] {
  override def visitPrimitiveType(ctx: PrimitiveTypeContext): Type =
    Type.Prim(TVirusParserPrimTypeVisitor.visit(ctx.primType()))

  override def visitParenType(ctx: ParenTypeContext): Type =
    visit(ctx.`type`())

  override def visitProduct(ctx: ProductContext): Type =
    Type.Product(visit(ctx.`type`(0)), visit(ctx.`type`(1)))

  override def visitSum(ctx: SumContext): Type =
    Type.Sum(visit(ctx.`type`(0)), visit(ctx.`type`(1)))

  override def visitFunction(ctx: FunctionContext): Type =
    Type.Function(visit(ctx.`type`(0)), visit(ctx.`type`(1)))
}

object TVirusParserSchemeVisitor extends TVirusParserBaseVisitor[Scheme] {
  override def visitPoly(ctx: PolyContext): Scheme =
    Scheme.Poly(
      ctx.IDENT().asScala.toSeq.map(_.getSymbol().getText()),
      TVirusParserTypeVisitor.visit(ctx.`type`())
    )

  override def visitMono(ctx: MonoContext): Scheme =
    Scheme.Mono(TVirusParserTypeVisitor.visit(ctx.`type`()))
}

object TVirusParserTBindVisitor extends TVirusParserBaseVisitor[TBind] {
  override def visitTBind(ctx: TBindContext): TBind =
    TBind(
      ctx.IDENT().getSymbol().getText(),
      Option(ctx.`type`()).map(TVirusParserTypeVisitor.visit)
    )
}

object TVirusParserSBindVisitor extends TVirusParserBaseVisitor[SBind] {
  override def visitSBind(ctx: SBindContext): SBind =
    SBind(
      ctx.IDENT().getSymbol().getText(),
      Option(ctx.scheme()).map(TVirusParserSchemeVisitor.visit)
    )
}

object TVirusParserCBindVisitor extends TVirusParserBaseVisitor[CBind] {
  override def visitCBind(ctx: CBindContext): CBind =
    CBind(
      ctx.IDENT().getSymbol().getText(),
      Option(ctx.`type`()).map(TVirusParserTypeVisitor.visit)
    )
}

object TVirusParserTypeDeclVisitor extends TVirusParserBaseVisitor[TypeDecl] {
  override def visitTypeDecl(ctx: TypeDeclContext): TypeDecl =
    TypeDecl(
      ctx.IDENT().getSymbol().getText(),
      ctx.cBind().asScala.toSeq.map(TVirusParserCBindVisitor.visit)
    )
}

object TVirusParserExprVisitor extends TVirusParserBaseVisitor[Expr] {
  override def visitVariable(ctx: VariableContext): Expr =
    Expr.Var(ctx.IDENT().getSymbol().getText())

  override def visitApplication(ctx: ApplicationContext): Expr =
    Expr.App(visit(ctx.expr(0)), visit(ctx.expr(1)))

  override def visitAbstraction(ctx: AbstractionContext): Expr =
    Expr.Abs(
      ctx.tBind().asScala.toSeq.map(TVirusParserTBindVisitor.visit),
      visit(ctx.expr())
    )

  override def visitPrimitiveOp(ctx: PrimitiveOpContext): Expr =
    Expr.Prim(TVirusParserPrimOpVisitor.visit(ctx.primOp()))

  override def visitLet(ctx: LetContext): Expr =
    Expr.Let(
      ctx
        .sBind()
        .asScala
        .toSeq
        .map(TVirusParserSBindVisitor.visit)
        .zip(ctx.expr().asScala.map(visit)),
      visit(ctx.expr().get(ctx.expr().size() - 1))
    )

  override def visitLiteralInteger(ctx: LiteralIntegerContext): Expr =
    Expr.LitInt(Integer.parseInt(ctx.LIT_INT().getText()))

  override def visitParenExpr(ctx: ParenExprContext): Expr =
    visit(ctx.expr())
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
      ctx.children.asScala.toSeq.map(t =>
        if (t.isInstanceOf[TypeDeclContext]) {
          TVirusParserTypeDeclVisitor.visit(t.asInstanceOf[TypeDeclContext])
        } else if (t.isInstanceOf[ValueDeclContext]) {
          TVirusParserValueDeclVisitor.visit(t.asInstanceOf[ValueDeclContext])
        } else {
          ???
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
