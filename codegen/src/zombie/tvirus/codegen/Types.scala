package zombie.tvirus.codegen

import zombie.tvirus.parser.Op

enum Type: 
  case TInt()
  case Auto()
  case Func(from: Type, target: Type)

enum CoreExpr:
  case CLam(name: String, ty: Type, body: CoreExpr)
  case CApp(fun: CoreExpr, arg: CoreExpr)
  case CInt(value: Int)
  case CCalc(left: CoreExpr, op: Op, right: CoreExpr)
  case CVar(name: String)
