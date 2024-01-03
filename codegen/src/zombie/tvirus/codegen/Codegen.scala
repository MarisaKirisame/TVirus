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

  p.tds.map(td =>
    td.cons.zipWithIndex.map((cb, idx) =>
      constructor_name_map.put(cb.name, idx)
    )
  )
}

def codegen_args(bindings: Seq[String], env: CodeGenEnv): String = {
  bracket(
    bindings
      .map(y =>
        s"const ${type_wrapper(codegen_type(tyck_expr(Expr.Var(y), env.tyck), env))}& ${y}"
      )
      .mkString(", ")
  )
}

def codegen_template_header_from_name(n: String, env: CodeGenEnv) = {
  env.tyck.var_map(n) match {
    case Type.TypeScheme(xs, _) => codegen_template_header(xs)
    case _                      => ""
  }
}

def codegen_vd_fwd(x: ValueDecl, env: CodeGenEnv): String = {
  if (x.x == "main") {
    ""
  } else {
    x.b match {
      case Expr.Abs(bindings, body) =>
        codegen_template_header_from_name(x.x, env) +
          type_wrapper(codegen_type(
            env.tyck.expr_map.get(body).get,
            env
          )) + " " + x.x + codegen_args(bindings, env) + ";"
    }
  }
}

def simple_pat_to_name(p: Pat): String = {
  p match {
    case Pat.Var(name) => name
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
        assert(false);
      }()"""
    }
    case Expr.App(Expr.Var(f), xs) => {
      f + bracket(xs.map(recur).mkString(", "))
    }
    case Expr.App(f, xs) => {
      codegen_bind(recur(f), f_ => f_ + bracket(xs.map(recur).mkString(", ")))
    }
    case Expr.Abs(bindings, body) => {
      s"""[=](${bindings.map(b => s"const ${type_wrapper(codegen_type(env.tyck.var_map(b), env))}& ${b}").mkString(", ")}){ return ${recur(body)}; }"""
    }
    case Expr.Cons(name, xs) => {
      (resolve(env.tyck.expr_map(x)) match {
        case Type.App(f, xs) => name + s"""<${xs.map(codegen_type(_, env)).mkString(", ")}>"""
        case Type.TyCons(_) => name
      }) + bracket(xs.map(recur).mkString(", "))
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
  resolve(x) match {
    case Type.Var(name, _) => name
    case Type.TyCons(name) => name
    case Type.Func(args, ret) =>
      s"std::function<${type_wrapper(recur(ret))}(${args.map(x => type_wrapper(recur(x))).mkString(", ")})>"
    case Type.App(Type.TyCons(name), xs) =>
      s"${name}<${xs.map(recur).mkString(", ")}>"
  }
}

def codegen_types(x: Seq[Type], env: CodeGenEnv): Seq[String] = {
  x.map(t => codegen_type(t, env))
}

def codegen_types_wrapped(x: Seq[Type], env: CodeGenEnv): Seq[String] = {
  x.map(t => type_wrapper(codegen_type(t, env)))
}

def tuplify(x: Seq[String]): String = {
  s"""std::tuple<${x.mkString(", ")}>"""
}

def codegen_template_header(xs: Seq[String]) = {
  if (xs.isEmpty) { "" }
  else { s"template<${xs.map(x => "typename " + x).mkString(", ")}>" }
}

def codegen_td_fwd(x: TypeDecl, env: CodeGenEnv): String = {
  s"""
  ${codegen_template_header(x.xs)}
  struct ${x.name};
  """
}

def variant_td(x: TypeDecl, env: CodeGenEnv): String = {
  s"std::variant<${x.cons.map(cb => tuplify(codegen_types_wrapped(cb.args, env))).mkString(", ")}>"
}

def codegen_td(x: TypeDecl, env: CodeGenEnv): String = {
  s"""
  ${codegen_template_header(x.xs)}
  struct ${x.name} { ${variant_td(x, env)} var; };
  ${codegen_constructors(x, env)}
  """
}

def codegen_constructors(x: TypeDecl, env: CodeGenEnv): String = {
  x.cons.zipWithIndex
    .map((cb, idx) => {
      val ret_type = type_wrapper(x.name + (if (x.xs.isEmpty) {""} else {s"<${x.xs.mkString(", ")}>"}))
      val name = cb.name
      val type_with_name = cb.args.map(ty => (ty, freshName()))
      val args = bracket(
        type_with_name
          .map((ty, n) => s"const ${type_wrapper(codegen_type(ty, env))}& ${n}")
          .mkString(", ")
      )
      s"""${codegen_template_header(x.xs)} ${ret_type} ${name}${args} {
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
    (x.b match {
      case Expr.Abs(bindings, body) => {
        codegen_template_header_from_name(x.x, env) +
          type_wrapper(codegen_type(
            env.tyck.expr_map.get(body).get,
            env
          )) + " " + x.x + codegen_args(bindings, env) +
          s"""{ 
          return ${codegen_expr(body, env)};
        }"""
      }
    })
  }
}

def codegen(x: Program): String = {
  val env = CodeGenEnv(x)
  """
  #include <memory>
  #include <variant>
  #include <functional>
  #include <cassert>
  """ ++
    x.tds.map(codegen_td_fwd(_, env)).mkString("\n") +
    x.tds.map(codegen_td(_, env)).mkString("\n") +
    x.vds.map(codegen_vd_fwd(_, env)).mkString("\n") +
    x.vds.map(codegen_vd(_, env)).mkString("\n") ++
    """
  int main() { }
  """
}

def compile(x: String) = {
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
  Process("clang-format -i output.cpp").!
  Process("cat output.cpp").!
  Process("g++ output.cpp").!
  println()
}
