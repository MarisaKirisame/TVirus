package zombie.tvirus.parser

import zombie.tvirus.parser.generated.*
import TVirusParser.*
import org.antlr.v4.runtime.CharStream
import org.antlr.v4.runtime.Vocabulary
import org.antlr.v4.runtime.CommonTokenStream

class TVirusExprVisitor extends TVirusBaseVisitor[Expr] {
  override def visitCalculation(ctx: CalculationContext): Expr =
    Expr.Calc(
      visit(ctx.expr(0)),
      ctx.op.getType() match {
        case TVirusLexer.MINUS => Op.MINUS
        case TVirusLexer.MULT  => Op.MULT
        case TVirusLexer.PLUS  => Op.PLUS
      },
      visit(ctx.expr(1))
    )

  override def visitParen(ctx: ParenContext): Expr = visit(ctx.expr())

  override def visitApplication(ctx: ApplicationContext): Expr =
    Expr.App(visit(ctx.expr(0)), visit(ctx.expr(1)))

  override def visitAbstraction(ctx: AbstractionContext): Expr =
    Expr.Abs(ctx.VAR().getSymbol().getText(), visit(ctx.expr()))

  override def visitVariable(ctx: VariableContext): Expr =
    Expr.Var(ctx.VAR().getSymbol().getText())

  override def visitInteger(ctx: IntegerContext): Expr =
    Expr.Int(Integer.parseInt(ctx.INT().getSymbol().getText()))
}

private def visitorInstance = TVirusExprVisitor()

def drive(source: CharStream): Expr = {
    val lexer = TVirusLexer(source)
    val stream = CommonTokenStream(lexer)
    val parser = TVirusParser(stream)
    visitorInstance.visit(parser.expr())
}
