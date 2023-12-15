package zombie.tvirus.codegen

import zombie.tvirus.parser.Op

def codegenTy(ty: Type): String = ty match
  case Type.TInt() => "int"
  case Type.Auto() => "auto"
  case Type.Func(from, target) => 
    s"std::function<${codegenTy(target)}(${codegenTy(from)})>"

def codegenOp(op: Op): String = op match
  case Op.PLUS => "+"
  case Op.MINUS => "-"
  case Op.MULT => "*"

def codegenExpr(core: CoreExpr): String = core match
  case CoreExpr.CLam(name, ty, body) => 
    s"[&](${codegenTy(ty)} $name) {return (${codegenExpr(body)});}"
  case CoreExpr.CApp(fun, arg) => 
    s"(${codegenExpr(fun)})(${codegenExpr(arg)})"
  case CoreExpr.CInt(value) => value.toString()
  case CoreExpr.CCalc(left, op, right) => 
    s"(${codegenExpr(left)}) ${codegenOp(op)} (${codegenExpr(right)})"
  case CoreExpr.CVar(name) => name


def codegen(core: CoreExpr): String = 
  s"#include <iostream>\n#include <functional>\nint main() {std::cout << (${codegenExpr(core)}) << std::endl; return 0;}"