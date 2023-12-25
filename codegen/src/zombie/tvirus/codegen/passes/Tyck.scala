package zombie.tvirus.codegen.passes

import cats.collections.DisjointSets
import zombie.tvirus.parser as p
import cats.kernel.Order

case class AExpr(e: Expr, t: p.Type)

enum Expr:
  case Var(name: String)

case class Constraint(name: String, ty: p.Type)

given Order[Constraint] = new Order[Constraint] {
  override def compare(x: Constraint, y: Constraint): Int =
    Order[String].compare(x.name, y.name)
}

case class Gamma(
    typeDecls: Seq[p.TypeDecl],
    terms: Map[String, AExpr],
    constraints: DisjointSets[Constraint]
)

