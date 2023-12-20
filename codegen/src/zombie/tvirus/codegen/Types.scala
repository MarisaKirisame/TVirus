package zombie.tvirus.codegen

import zombie.tvirus.parser.{PrimOp, Type}

enum CoreExpr:
  case Lam(name: String, ty: Type, body: CoreExpr)
  case App(fun: CoreExpr, arg: CoreExpr)
  case LitInt(inner: Int)
  case Prim(op: PrimOp)
  case Var(name: String)


enum CoreDecl:
  // !!!: the Scheme in SBind is temporarily ignored
  case ValueDecl(x: String, b: CoreExpr)

  case MainDecl(b: CoreExpr)

case class CoreProgram(decls: Seq[CoreDecl])
