package zombie.tvirus.codegen

import zombie.tvirus.parser.Op

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
}