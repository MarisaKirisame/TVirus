package zombie.tvirus.codegen.passes

import zombie.tvirus.parser.*

enum Type:
  case Int
  case Var(name: String)
  case Func(from: Type, target: Type)

enum Core:
  case Lam(name: String, ty: Type, body: Core)
  case App(fun: Core, arg: Core)
  case Int(value: Int)
  case Calc(left: Core, op: Op, right: Core)
  case Var(name: String)

extension (env: Map[String, Type])
  def lookup(name: String) = {}