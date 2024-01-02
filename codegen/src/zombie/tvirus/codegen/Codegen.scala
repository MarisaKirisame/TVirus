package zombie.tvirus.codegen

import java.io.FileWriter
import java.io.IOException
import scala.sys.process._
import collection.mutable
import zombie.tvirus.parser.*

class CodeGenEnv(p: Program) {
  val tyck: TyckEnv = tyck_program(p)
  val constructor_name_map: mutable.Map[String, Int] =
    mutable.Map[String, Int]()

  p.decls.map(_ match {
    case vd: ValueDecl => {}
    case td: TypeDecl => {
      td.cons.zipWithIndex.map((cb, idx) =>
        constructor_name_map.put(cb.name, idx)
      )
    }
  })
}

def codegen_args(bindings: Seq[String], env: CodeGenEnv): String = {
  bracket(
    bindings
      .map(y =>
        s"const ${codegen_type(tyck_expr(Expr.Var(y), env.tyck), env)}& ${y}"
      )
      .mkString(", ")
  )
}

def codegen_vd_fwd(x: ValueDecl, env: CodeGenEnv): String = {
  if (x.x == "main") {
    ""
  } else {
    x.b match {
      case Expr.Abs(bindings, body) =>
        codegen_type(
          env.tyck.expr_map.get(body).get,
          env
        ) + " " + x.x + codegen_args(bindings, env) + ";"
    }
  }
}

def codegen_case(name: String, c: (Pat, Expr), env: CodeGenEnv): String = {
  c(0) match {
    case Pat.Wildcard => codegen_expr(c(1), env)
    case Pat.Cons(cons_name, xs) => {
      val idx = env.constructor_name_map.get(cons_name).get
      codegen_bind(
        name,
        x =>
          s"""if (${x}.var.index() == ${idx}) { 
          ${xs.map(simple_pat_to_name)
              .zipWithIndex
              .map((n, arg_idx) =>
                s"auto ${n} = std::get<${arg_idx}>(std::get<${idx}>(${x}.var));"
              )
              .mkString("\n")}
          return ${codegen_expr(c(1), env)};
        }"""
      )
    }
  }
}

def codegen_bind(x: String, y: String => String): String = {
  y(s"(*${x})")
}

def codegen_expr(x: Expr, env: CodeGenEnv): String = {
  val recur = x => codegen_expr(x, env)
  x match {
    case Expr.Var(name) => name
    case Expr.Match(x, cases) => {
      val fn = freshName()
      s"""[&](){
        auto ${fn} = ${recur(x)};
        ${cases.map(c => codegen_case(fn, c, env)).mkString("\n")}
      }()"""
    }
    case Expr.App(f, xs) => {
      codegen_bind(recur(f), f_ => f_ + bracket(xs.map(recur).mkString(", ")))
    }
    case Expr.Abs(bindings, body) => {
      s"""[](){ return ${recur(body)}; }"""
    }
    case Expr.Cons(name, xs) => {
      name + bracket(xs.map(recur).mkString(", "))
    }
    case Expr.Let(bs, body) => {
      s"""[&](){
        ${bs.map((n, v) => s"auto ${n} = ${recur(v)};").mkString("\n")} 
        return ${recur(body)};
      }()"""
    }
  }
}

def type_wrapper(x: String): String = {
  s"std::shared_ptr<${x}>"
}

def codegen_type(x: Type, env: CodeGenEnv): String = {
  val recur = y => codegen_type(y, env)
  type_wrapper(resolve(x) match {
    case Type.Var(name, _) => name
    case Type.TyCons(name) => name
    case Type.Func(args, ret) =>
      s"std::function<${recur(ret)}(${args.map(recur).mkString(", ")})>"
  })
}

def codegen_types(x: Seq[Type], env: CodeGenEnv): Seq[String] = {
  x.map(t => codegen_type(t, env))
}

def tuplify(x: Seq[String]): String = {
  s"""std::tuple<${x.mkString(", ")}>"""
}

def codegen_td_fwd(x: TypeDecl, env: CodeGenEnv): String = {
  s"""  
  struct ${x.name};
  """
}

def variant_td(x: TypeDecl, env: CodeGenEnv): String = {
  s"std::variant<${x.cons.map(cb => tuplify(codegen_types(cb.args, env))).mkString(", ")}>"
}
def codegen_td(x: TypeDecl, env: CodeGenEnv): String = {
  s"struct ${x.name} { ${variant_td(x, env)} var; };"
}

def codegen_constructors(x: TypeDecl, env: CodeGenEnv): String = {
  x.cons.zipWithIndex
    .map((cb, idx) => {
      val ret_type = codegen_type(Type.Var(x.name, None), env)
      val name = cb.name
      val type_with_name = cb.args.map(ty => (ty, freshName()))
      val args = bracket(
        type_with_name
          .map((ty, n) => s"const ${codegen_type(ty, env)}& ${n}")
          .mkString(", ")
      )
      s"""${ret_type} ${name}${args} {
       return std::make_shared<${x.name}>(${x.name}{.var=${variant_td(
          x,
          env
        )}{std::in_place_index<${idx}>, ${type_with_name
          .map(_(1))
          .mkString(", ")}}});
    }"""
    })
    .mkString("\n")
}

def codegen_vd(x: ValueDecl, env: CodeGenEnv): String = {
  if (x.x == "main") {
    ""
  } else {
    x.b match {
      case Expr.Abs(bindings, body) => {
        codegen_type(
          env.tyck.expr_map.get(body).get,
          env
        ) + " " + x.x + codegen_args(bindings, env) +
          s"""{ 
          return ${codegen_expr(body, env)};
        }"""
      }
    }
  }
}

def codegen(x: Program): String = {
  val env = CodeGenEnv(x)
  """
  #include <memory>
  #include <variant>
  #include <functional>
  """ ++
    x.decls
      .map(_ match {
        case vd: ValueDecl => codegen_vd_fwd(vd, env)
        case td: TypeDecl  => codegen_td_fwd(td, env)
      })
      .mkString("\n") +
    x.decls
      .map(_ match {
        case vd: ValueDecl => ""
        case td: TypeDecl =>
          codegen_td(td, env) + "\n" + codegen_constructors(td, env)
      })
      .mkString("\n") +
    x.decls
      .map(_ match {
        case vd: ValueDecl => codegen_vd(vd, env)
        case td: TypeDecl  => ""
      })
      .mkString("\n") ++
    """
  int main() { }
  """
}

def compile(x: String) = {
  println(x)
  try {
    Process("rm output.cpp").!
    // todo use os-lib
    val fileWriter = new FileWriter("output.cpp")
    fileWriter.write(x)
    fileWriter.close()
  } catch {
    case e: IOException =>
      println("fail to write into target-file" + e.getMessage)
  }
  Process("g++ output.cpp").!
  println()
}