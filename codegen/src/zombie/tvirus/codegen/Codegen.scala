package zombie.tvirus.codegen

import zombie.tvirus.parser.{PrimOp, Type}
import zombie.tvirus.parser.PrimType

def codegenTy(ty: Type): String = ty match
  case Type.Prim(t) => t match
    case PrimType.INT => "int"
    case PrimType.BOOL => "bool"
  
  case Type.Func(x, r) => 
    s"std::function<${codegenTy(r)}(${codegenTy(x)})>"
  case Type.Var(name) => "auto"

def codegenExpr(core: CoreExpr): String = core match
  case CoreExpr.Lam(name, ty, body) => 
    s"[=](const ${codegenTy(ty)} &${name}) { return ${codegenExpr(body)}; }"
  case zombie.tvirus.codegen.CoreExpr.App(fun, arg) => 
    s"(${codegenExpr(fun)})(${codegenExpr(arg)})"
  case CoreExpr.LitInt(inner) => inner.toString
  // case CoreExpr.Prim(op) => {
  //   val op_str = op match
  //     case PrimOp.ADD => "+"
  //     case PrimOp.MINUS => "-"
  //     case PrimOp.MUL => "*"
  //     case PrimOp.DIV => "/"
  //     case PrimOp.EQ => "=="
  //     case PrimOp.NE => "!="
  //     case PrimOp.GT => ">"
  //     case PrimOp.LT => "<"
  //     case PrimOp.GE => ">="
  //     case PrimOp.LE => "<="

  //   s"([](const auto &a) { return [=](const auto &b) {return a ${op_str} b;};})"
  // }
  case CoreExpr.Var(name) => name

def codegen(core: CoreProgram): String = {
  val headers = "#include <iostream>\n#include <functional>\n#include <variant>\n"

  val statements = core.decls.map {
    case CoreDecl.ValueDecl(x, b, _ty) => {
      val ty = codegenTy(_ty)
      val expr = codegenExpr(b)
      
      s"${ty} ${x} = ${expr};"
    }
    case CoreDecl.MainDecl(b) => {
      val expr = codegenExpr(b)
      s"int main() { std::cout << ${expr} << std::endl; }"
    }
  }

  s"${headers}\n${statements.mkString("\n")}"
}