package zombie.tvirus.codegen

import zombie.tvirus.parser.{Expr, Type, PrimOp, PrimType, TBind, Program}
import zombie.tvirus.parser.ValueDecl

class TypeTable:
  private var table: Map[String, Type] = Map()

  def visit(name: String): Type = table.getOrElse(name, Type.Var(name)) match
    case Type.Var(n) => {
      if n == name then
        Type.Var(n)
      else {
        val v = visit(n)
        table = table.updated(name, v)
        v
      }
    }
    case value => value
  
  def freeVariables(): Set[String] = 
    table.keys.map { k => visit(k) }.collect {
      case Type.Var(n) => n
    }.toSet
  
  def canonical(ty: Type): Type = ty match
    case Type.Prim(_) => ty
    case Type.Var(name) => visit(name)
    case Type.Func(from, target) => 
      Type.Func(canonical(from), canonical(target))
  
  def set(name: String, value: Type): Unit = {
    visit(name) match
      case Type.Var(n) =>
        table = table.updated(n, value)
      case _ => 
        println("You can't set value for an established value")
  }

  def freshen(name: String): String = {
    if !table.contains(name) then {
      table = table.updated(name, Type.Var(name))
      name
    }
    else 
      freshen(name + "!")
  }
end TypeTable

type Env = Map[String, (CoreExpr, Type)]

def unifyType(a: Type, b: Type, table: TypeTable): Boolean = {
  def find(ty: Type): Type = ty match 
    case Type.Var(n) => table.visit(n)
    case _ => ty
  
  (find(a), find(b)) match
    case (Type.Var(a), tb) => {
      table.set(a, tb)
      true
    }
    case (ta, Type.Var(b)) => {
      table.set(b, ta)
      true
    }
    case (Type.Prim(pr1), Type.Prim(pr2)) => pr1 == pr2
    case (Type.Func(fromA, targetA), Type.Func(fromB, targetB)) => 
      // since if we failed to union two types, the compilation terminated,
      // so we don't care the modification on the table when only one part of the union success.
      unifyType(fromA, fromB, table) && unifyType(targetA, targetB, table)
    case _ => false
}

def compileWithEnv(expr: Expr, env: Env, type_table: TypeTable)
  : Either[String, (CoreExpr, Type)] = {
    def failIf(b: Boolean, msg: String): Either[String, Unit] = {
      if b then 
        Left(msg)
      else
        Right(())
    }

    expr match
      case Expr.Var(name) => 
        env.get(name).toRight(s"Can't find variable ${name}")
      case Expr.LitInt(int) => {
        Right((CoreExpr.LitInt(int), Type.Prim(PrimType.INT)))
      }
      case Expr.Prim(op) => {
        val TInt = Type.Prim(PrimType.INT)
        if Seq(PrimOp.ADD, PrimOp.MINUS, PrimOp.MUL).contains(op) then
          Right((CoreExpr.Prim(op), Type.Func(TInt, Type.Func(TInt, TInt))))
        else
          Left(s"Currently unsupported operator ${op}")
      }

      case Expr.App(f, x) => {
        val target = type_table.freshen("target")

        for 
          fun <- compileWithEnv(f, env, type_table)
          arg <- compileWithEnv(x, env, type_table)
          _ <- failIf(!unifyType(fun._2, Type.Func(arg._2, Type.Var(target)), type_table), 
                      s"fail to unify ${fun._2} as a funtion type with ${arg._2} from")
        yield (CoreExpr.App(fun._1, arg._1), type_table.visit(target))
      }

      case Expr.Abs(Nil, body) => compileWithEnv(body, env, type_table)

      case Expr.Abs(TBind(name, ty) +: tail, body) => {
        val arg = type_table.freshen(name)
        val newPair = ty match
          case None => (CoreExpr.Var(arg), Type.Var(arg))
          case Some(value) => (CoreExpr.Var(arg), value)
        val newEnv = env.+((name, newPair))

        for 
          body_result <- compileWithEnv(body, newEnv, type_table)
        yield (CoreExpr.Lam(name, Type.Var(arg), body_result._1),
               Type.Func(Type.Var(arg), body_result._2))
      }
  }

def canonize_type(core: CoreExpr, type_table: TypeTable): CoreExpr = core match
  case CoreExpr.Lam(name, ty, body) => 
    CoreExpr.Lam(name, type_table.canonical(ty), canonize_type(body, type_table))
  case CoreExpr.App(fun, arg) =>
    CoreExpr.App(canonize_type(fun, type_table), canonize_type(arg, type_table))
  case CoreExpr.LitInt(value) => core
  case CoreExpr.Var(name) => core
  case CoreExpr.Prim(op) => core

def compileExpr(expr: Expr, env: Env, type_table: TypeTable): Either[String, (CoreExpr, Type)] = 
  var next_type_table = TypeTable()
  for 
    result <- compileWithEnv(expr, env, type_table)
  yield (canonize_type(result._1, type_table), type_table.canonical(result._2))

def compile(prog: Program): Either[String, CoreProgram] = {
  var type_table = TypeTable()
  var env = Map[String, (CoreExpr, Type)]()

  var core_exprs = Seq[CoreDecl]()
  var freeTypeVars = Set[String]()

  for 
    decl <- prog.decls
  do {
    decl match
      case ValueDecl(x, b) => {
        // scheme is ignore for now
        // we should copy type_table here
        compileExpr(b, env, type_table) match
          case Left(msg) => return Left(msg)
          case Right((core, ty)) => {
            core_exprs = core_exprs :+ (
              if x.name == "main" then
                CoreDecl.MainDecl(core)
              else 
                CoreDecl.ValueDecl(x.name, core)
            )
            env = env.updated(x.name, (core, ty))
            freeTypeVars = freeTypeVars ++ type_table.freeVariables()
            type_table = TypeTable()
            for v <- freeTypeVars do {
              type_table.set(v, Type.Var(v))
            }
          }
      }
  }

  Right(CoreProgram(core_exprs))
}