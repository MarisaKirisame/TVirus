package zombie.tvirus.codegen

import zombie.tvirus.parser.{Op, Expr}

@main
def main(): Unit = {
  val core = 
    CoreExpr.CApp(
      CoreExpr.CApp(
        CoreExpr.CLam("f", Type.Func(Type.TInt(), Type.TInt()), 
          CoreExpr.CLam("x", Type.TInt(), 
            CoreExpr.CApp(
              CoreExpr.CVar("f"),
              CoreExpr.CVar("x")
            )
          )
        ),
        CoreExpr.CLam("x", Type.TInt(), 
          CoreExpr.CCalc(CoreExpr.CInt(21), Op.PLUS, CoreExpr.CVar("x"))
        )
      ),
      CoreExpr.CInt(21)
    )
  
  println(codegen(core))
    
  val expr = Expr.App(
    Expr.Abs("x",
      Expr.Calc(Expr.Var("x"), Op.MINUS, Expr.Int(8))
    ),
    Expr.Int(50)
  )

  val exprFail = Expr.App(
    Expr.Int(42),
    Expr.Int(42)
  )

  compile(expr) match
    case Left(msg) => println(msg)
    case Right(core) => println(codegen(core))

  compile(exprFail) match
    case Left(msg) => println(msg)
    case Right(core) => println(codegen(core))
}