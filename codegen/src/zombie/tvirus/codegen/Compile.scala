package zombie.tvirus.codegen

import zombie.tvirus.parser.{Expr, Op}

class TypeTable:
  private var table: Map[String, Type] = Map()

  def visit(name: String): Type = table.getOrElse(name, Type.TVar(name)) match
    case Type.TVar(n) => {
      if n == name then
        Type.TVar(n)
      else {
        val v = visit(n)
        table = table.updated(name, v)
        v
      }
    }
    case value => value
  
  def canonical(ty: Type): Type = ty match
    case Type.TInt() => Type.TInt()
    case Type.TVar(name) => visit(name)
    case Type.Func(from, target) => 
      Type.Func(canonical(from), canonical(target))
  
  def set(name: String, value: Type): Unit = {
    visit(name) match
      case Type.TVar(n) =>
        table = table.updated(n, value)
      case _ => 
        println("You can't set value for an established value")
  }

  def freshen(name: String): String = {
    if table.get(name) == None then {
      table.updated(name, Type.TVar(name))
      name
    }
    else 
      freshen(name + "!")
  }
end TypeTable

type Env = Map[String, (CoreExpr, Type)]

def unifyType(a: Type, b: Type, table: TypeTable): Boolean = {
  def find(ty: Type): Type = ty match 
    case Type.TVar(n) => table.visit(n)
    case _ => ty
  
  (find(a), find(b)) match
    case (Type.TVar(a), tb) => {
      table.set(a, tb)
      true
    }
    case (ta, Type.TVar(b)) => {
      table.set(b, ta)
      true
    }
    case (Type.TInt(), Type.TInt()) => true
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
      case Expr.Int(int) => {
        Right((CoreExpr.CInt(int), Type.TInt()))
      }
      case Expr.Calc(l, op, r) => 
        for 
          left <- compileWithEnv(l, env, type_table)
          right <- compileWithEnv(r, env, type_table)
          _ <- failIf(!unifyType(left._2, Type.TInt(), type_table),
                      s"fail to unify ${left._2} with Int")
          _ <- failIf(!unifyType(right._2, Type.TInt(), type_table), 
                      s"fail to unify ${right._2} with Int")
        yield (CoreExpr.CCalc(left._1, op, right._1), Type.TInt())

      case Expr.App(f, x) => {
        val target = type_table.freshen("target")

        for 
          fun <- compileWithEnv(f, env, type_table)
          arg <- compileWithEnv(x, env, type_table)
          _ <- failIf(!unifyType(fun._2, Type.Func(Type.TVar(target), arg._2), type_table), 
                      s"fail to unify ${fun._2} as a funtion type with ${arg._2} from")
        yield (CoreExpr.CApp(fun._1, arg._1), type_table.visit(target))
      }

      case Expr.Abs(x, body) => {
        val arg = type_table.freshen(x)
        val newEnv = env.+((x, (CoreExpr.CVar(x), Type.TVar(arg))))

        for 
          body_result <- compileWithEnv(body, newEnv, type_table)
        yield (CoreExpr.CLam("x", Type.TVar(arg), body_result._1), 
               Type.Func(Type.TVar(arg), body_result._2))
      }
  }

def canonize_type(core: CoreExpr, type_table: TypeTable): CoreExpr = core match
  case CoreExpr.CLam(name, ty, body) => 
    CoreExpr.CLam(name, type_table.canonical(ty), canonize_type(body, type_table))
  case CoreExpr.CApp(fun, arg) =>
    CoreExpr.CApp(canonize_type(fun, type_table), canonize_type(arg, type_table))
  case CoreExpr.CInt(value) => core
  case CoreExpr.CCalc(left, op, right) => 
    CoreExpr.CCalc(canonize_type(left, type_table), op, canonize_type(right, type_table))
  case CoreExpr.CVar(name) => core

def compile(expr: Expr): Either[String, CoreExpr] = 
  var type_table = TypeTable()
  for 
    result <- compileWithEnv(expr, Map(), type_table)
  yield canonize_type(result._1, type_table)
