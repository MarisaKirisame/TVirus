package zombie.tvirus.codegen.passes

import cats.collections.DisjointSets
import zombie.tvirus.parser.{Expr as PExpr, Type as PType, Scheme as PScheme}

case class AExpr(e: Expr, t: Type)

enum Expr:
  case Var(name: String)
