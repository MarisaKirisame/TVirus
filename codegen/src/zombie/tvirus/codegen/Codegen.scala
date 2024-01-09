package zombie.tvirus.codegen

import java.io.FileWriter
import java.io.IOException
import scala.sys.process._
import collection.mutable
import zombie.tvirus.parser.*

trait BackEnd {
  def type_wrapper(x: String): String
  def val_wrapper(t: String, v: String): String
  def header: String
  def handle_constructor(x: TypeDecl): String
  def codegen_binds(x: Seq[String], y: Seq[String] => String): String
  def codegen_bind(x: String, y: String => String): String = {
    codegen_binds(Seq(x), x_ => y(x_(0)))
  }
}

class ZombieBackEnd extends BackEnd {
  def type_wrapper(x: String): String = {
    s"Zombie<${x}>"
  }
  def header: String = {
    s"""
    #include <zombie/zombie.hpp>
    IMPORT_ZOMBIE(default_config)

    template<>
    struct GetSize<int64_t> {
      size_t operator()(const int64_t& x) { return sizeof(x); }
    };
    template<>
    struct GetSize<bool> {
      size_t operator()(const bool& x) { return sizeof(x); }
    };
    template<typename X, typename... Y>
    struct GetSize<std::function<X(Y...)>> {
      size_t operator()(const auto& x) { }
    };
    template<typename X>
    X fail() {
      assert(false);
    }
    """
  }
  def handle_constructor(x: TypeDecl): String = {
    s"""
    template<${x.xs.map(n => s"typename ${n}").mkString(", ")}>
    struct GetSize<${x.name}${
        if (x.xs.isEmpty) { "" }
        else { s"<${x.xs.mkString(", ")}>" }
      }> {
      size_t operator()(const auto& x) {

      }
    };
    """
  }
  def codegen_binds(x: Seq[String], y: Seq[String] => String): String = {
    val fn = x.map(_ => freshName())
    s"""bindZombie([=](${fn
        .map(n => s"const auto& ${n}")
        .mkString(", ")}){ return ${y(fn)}; }, ${x.mkString(", ")})"""
  }
  def val_wrapper(t: String, v: String): String = {
    s"""Zombie<${t}>(${v})"""
  }
}

class NoZombieBackEnd extends BackEnd {
  def type_wrapper(x: String): String = {
    s"std::shared_ptr<${x}>"
  }
  def header: String = {
    ""
  }
  def handle_constructor(x: TypeDecl): String = {
    ""
  }
  def codegen_binds(x: Seq[String], y: Seq[String] => String): String = {
    val binds = x.map(x => (x, freshName()))
    stmts_to_expr(s"""
    ${binds.map((x, n) => s"auto ${n} = *${x};").mkString("\n")}
    return ${y(binds.map((x, n) => s"${n}"))};
    """)
  }
  def val_wrapper(t: String, v: String): String = {
    s"""std::make_shared<${t}>(${v})"""
  }
}

val UseZombie = true
val BE = if (UseZombie) { ZombieBackEnd() }
else { NoZombieBackEnd() }

class CodeGenEnv(p: Program) {
  val tyck: TyckEnv = tyck_program(p)
  val constructor_name_map: mutable.Map[String, Int] =
    mutable.Map[String, Int]()
  p.tds.map(td =>
    td.cons.zipWithIndex.map((cb, idx) =>
      constructor_name_map.put(cb.name, idx)
    )
  )
  val global_funcs: Set[String] = {
    p.vds.map(vd => vd.x).toSet
  }
}

def codegen_args(bindings: Seq[String], env: CodeGenEnv): String = {
  bracket(
    bindings
      .map(y =>
        s"const ${BE.type_wrapper(codegen_type(tyck_expr(Expr.Var(y), env.tyck), env))}& ${y}"
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
          BE.type_wrapper(
            codegen_type(
              env.tyck.expr_map.get(body).get,
              env
            )
          ) + " " + x.x + codegen_args(bindings, env) + ";"
    }
  }
}

def simple_pat_to_name(p: Pat): String = {
  p match {
    case Pat.Var(name) => name
  }
}

def stmts_to_expr(stmts: String): String = {
  s"""[&](){${stmts}}()"""
}

def codegen_case(name: String, c: (Pat, Expr), env: CodeGenEnv): String = {
  c(0) match {
    case Pat.Wildcard => codegen_expr(c(1), env)
    case Pat.Cons(cons_name, xs) => {
      val idx = env.constructor_name_map.get(cons_name).get
      s"""if (${name}.var.index() == ${idx}) { 
          ${xs
          .map(simple_pat_to_name)
          .zipWithIndex
          .map((n, arg_idx) =>
            s"auto ${n} = std::get<${arg_idx}>(std::get<${idx}>(${name}.var));"
          )
          .mkString("\n")}
          return ${codegen_expr(c(1), env)};
        }"""
    }
  }
}

def codegen_primop(l: String, op: PrimOp, r: String) = {
  op match {
    case PrimOp.EQ    => s"${l} == ${r}"
    case PrimOp.MINUS => s"${l} - ${r}"
    case PrimOp.ADD   => s"${l} + ${r}"
    case PrimOp.MUL   => s"${l} * ${r}"
    case PrimOp.DIV   => s"${l} / ${r}"
    case PrimOp.MOD   => s"${l} % ${r}"
    case PrimOp.LT    => s"${l} < ${r}"
    case PrimOp.LE    => s"${l} <= ${r}"
    case PrimOp.GT    => s"${l} > ${r}"
    case PrimOp.GE    => s"${l} >= ${r}"
  }
}

def codegen_expr(x: Expr, env: CodeGenEnv): String = {
  val recurse = x => codegen_expr(x, env)
  x match {
    case Expr.Var(name) => name
    case Expr.Match(x, cases) => {
      BE.codegen_bind(
        recurse(x),
        x_ =>
          stmts_to_expr(
            cases.map(c => codegen_case(x_, c, env)).mkString("\n") +
              "assert(false);"
          )
      )
    }
    case Expr.App(Expr.Var(f), xs) => {
      if (env.global_funcs.contains(f)) {
        f + bracket(xs.map(recurse).mkString(", "))
      } else {
        BE.codegen_bind(f, f_ => f_ + bracket(xs.map(recurse).mkString(", ")))
      }
    }
    case Expr.App(f, xs) => {
      BE.codegen_bind(
        recurse(f),
        f_ => f_ + bracket(xs.map(recurse).mkString(", "))
      )
    }
    case Expr.Abs(bindings, body) => {
      BE.val_wrapper(
        codegen_type(
          env.tyck.expr_map(x),
          env
        ),
        s"""[=](${bindings
            .map(b =>
              s"const ${BE.type_wrapper(codegen_type(env.tyck.var_map(b), env))}& ${b}"
            )
            .mkString(", ")}){ return ${recurse(body)}; }"""
      )
    }
    case Expr.Cons(name, xs) => {
      (resolve(env.tyck.expr_map(x)) match {
        case Type.App(f, xs) =>
          name + s"""<${xs.map(codegen_type(_, env)).mkString(", ")}>"""
        case Type.TyCons(_) => name
      }) + bracket(xs.map(recurse).mkString(", "))
    }
    case Expr.Let(bs, body) => {
      stmts_to_expr(s"""
        ${bs.map((n, v) => s"auto ${n} = ${recurse(v)};").mkString("\n")} 
        return ${recurse(body)};
      """)
    }
    case Expr.LitInt(x) => {
      BE.val_wrapper("int64_t", x.toString)
    }
    case Expr.If(i, t, e) =>
      BE.codegen_bind(
        recurse(i),
        i_ => s"${i_} ? ${recurse(t)} : ${recurse(e)}"
      )
    case Expr.Prim(l, op, r) => {
      BE.codegen_binds(
        Seq(recurse(l), recurse(r)),
        xs =>
          BE.val_wrapper(
            codegen_type(env.tyck.expr_map(x), env),
            codegen_primop(xs(0), op, xs(1))
          )
      )
    }
    case Expr.Fail() => {
      s"fail<${codegen_type(env.tyck.expr_map(x), env)}>()"
    }
  }
}

def cref_wrapper(x: String): String = {
  s"const ${x}&"
}

def codegen_type(x: Type, env: CodeGenEnv): String = {
  val recurse = y => codegen_type(y, env)
  resolve(x) match {
    case Type.Var(name, _) => name
    case Type.TyCons(name) => name
    case Type.Func(args, ret) =>
      s"std::function<${BE.type_wrapper(recurse(ret))}(${args
          .map(x => cref_wrapper(BE.type_wrapper(recurse(x))))
          .mkString(", ")})>"
    case Type.App(Type.TyCons(name), xs) =>
      s"${name}<${xs.map(recurse).mkString(", ")}>"
    case Type.Prim(PrimType.INT) => "int64_t"
    case Type.Prim(PrimType.BOOL) => "bool"
  }
}

def codegen_types(x: Seq[Type], env: CodeGenEnv): Seq[String] = {
  x.map(t => codegen_type(t, env))
}

def codegen_types_wrapped(x: Seq[Type], env: CodeGenEnv): Seq[String] = {
  x.map(t => BE.type_wrapper(codegen_type(t, env)))
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
      val ret_type_unwrapped = x.name + (if (x.xs.isEmpty) { "" }
                                         else {
                                           s"<${x.xs.mkString(", ")}>"
                                         })
      val ret_type = BE.type_wrapper(ret_type_unwrapped)
      val name = cb.name
      val type_with_name = cb.args.map(ty => (ty, freshName()))
      val args = bracket(
        type_with_name
          .map((ty, n) =>
            s"const ${BE.type_wrapper(codegen_type(ty, env))}& ${n}"
          )
          .mkString(", ")
      )
      s"""${codegen_template_header(x.xs)} ${ret_type} ${name}${args} {
       return ${BE.val_wrapper(
          ret_type_unwrapped,
          ret_type_unwrapped + s"""{
          .var=${variant_td(
              x,
              env
            )}{std::in_place_index<${idx}>, ${type_with_name
              .map(_(1))
              .mkString(", ")}}
       }"""
        )};
    }"""
    })
    .mkString("\n") + BE.handle_constructor(x)
}

def codegen_vd(x: ValueDecl, env: CodeGenEnv): String = {
  if (x.x == "main") {
    s"int main() { ${codegen_expr(x.b, env)}; }"
  } else {
    (x.b match {
      case Expr.Abs(bindings, body) => {
        codegen_template_header_from_name(x.x, env) +
          BE.type_wrapper(
            codegen_type(
              env.tyck.expr_map.get(body).get,
              env
            )
          ) + " " + x.x + codegen_args(bindings, env) +
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
    BE.header ++
    x.tds.map(codegen_td_fwd(_, env)).mkString("\n") +
    x.tds.map(codegen_td(_, env)).mkString("\n") +
    x.vds.map(codegen_vd_fwd(_, env)).mkString("\n") +
    x.vds.map(codegen_vd(_, env)).mkString("\n")
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
  Process("g++ -std=c++20 output.cpp").!
  println()
}
