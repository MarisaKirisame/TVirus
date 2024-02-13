package zombie.tvirus.codegen

import java.io.FileWriter
import java.io.IOException
import scala.sys.process._
import collection.mutable
import zombie.tvirus.parser.*

enum Value:
  case Expr(x: String)
  case Stmts(x: String)

  def toExpr = {
    this.match
      case Expr(x)  => x
      case Stmts(x) => s"[&](){${x}}()"
  }

  def toStmts = {
    this.match
      case Expr(x)  => s"return ${x};"
      case Stmts(x) => x
  }

  def ++(rhs: Value) = {
    this.match
      case Expr(x)  => this
      case Stmts(x) => Value.Stmts(x ++ "\n" ++ rhs.toStmts)
  }

  override def toString = toExpr

def bracket(s: String) = "(" ++ s ++ ")"

def cbracket(x: String) = "{" + x + "}"

//def lambda(capture: String, args: String, body: String) = s"[${capture}](${args}){${body}}"
def lambda(capture: String, args: String, body: String) = s"[=](${args}){${body}}"

trait BackEnd {
  def type_wrapper(x: String): String
  def return_type_wrapper(x: String): String
  def val_wrapper_raw(t: String, v: String): String
  def val_wrapper(t: String, v: String): Value = {
    Value.Expr(val_wrapper_raw(t, v))
  }
  def header: String
  def handle_constructor(x: TypeDecl): String
  def codegen_binds_raw(captured: Set[String], x: Seq[String], y: Seq[String] => Value): Value
  def codegen_binds(is_tail: Boolean, captured: Set[String], x: Seq[Value], y: Seq[String] => Value): Value = {
    if (is_tail) {
      val xname = x.map(_ => freshName())
      val args = lambda(captured.mkString(","), xname.map(n => "const auto& " ++ n).mkString(", "), y(xname).toStmts) +: x.map(_.toExpr)
      Value.Expr(s"TailCall(${args.mkString(", ")})")
    } else {
      codegen_binds_raw(captured, x.map(_.toExpr), y)
    }
  }
  def codegen_bind(is_tail: Boolean, captured: Set[String], x: Value, y: String => Value): Value = {
    codegen_binds(is_tail, captured, Seq(x), x_ => y(x_(0)))
  }
}

class ZombieBackEnd(limit: Long) extends BackEnd {
  def type_wrapper(x: String): String = {
    s"Zombie<${x}>"
  }
  def return_type_wrapper(x: String): String = {
    s"TCZombie<${x}>"
  }
  def header: String = {
    s"""
    #include <zombie/zombie.hpp>
    #include <iostream>
    IMPORT_ZOMBIE(default_config)

    struct Init {
      Init() {
        Trailokya::get_trailokya().each_step = [](){ 
          ${if (limit == 0)  {
             "" 
            } else {
              s"""while (allocated > ${limit}) {
                Trailokya::get_trailokya().reaper.murder();
              }"""
            }}
          record();
        };
      }
    } init;

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
      size_t operator()(const auto& x) { return sizeof(void*); }
    };
    template<typename X>
    X fail() {
      assert(false);
    }
    template<typename Ret, typename... X>
    TCZombie<Ret> FuncApp(const Zombie<std::function<TCZombie<Ret>(const Zombie<X>&...)>>& f, const Zombie<X>&... x) {
      return TailCall([=](const std::function<TCZombie<Ret>(const Zombie<X>&...)>& func) { return func(x...); }, f);
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
        return 8;
      }
    };
    """
  }
  def codegen_binds_raw(captured: Set[String], x: Seq[String], y: Seq[String] => Value): Value = {
    val fn = x.map(_ => freshName())
    if (captured.size != 0) {
      println(captured)
      assert(false)
    }
    Value.Expr(s"""bindZombie(${lambda(captured.mkString(","), fn
        .map(n => "const auto& " ++ n).mkString(", "), y(fn).toStmts)}, ${x.mkString(", ")})""")
  }
  def val_wrapper_raw(t: String, v: String): String = {
    s"""Zombie<${t}>(${v})"""
  }
}

class NoZombieBackEnd extends BackEnd {
  def type_wrapper(x: String): String = {
    s"std::shared_ptr<${x}>"
  }
  def return_type_wrapper(x: String): String = {
    s"TCSP<${x}>"
  }
  def header: String = {
    s"""
    template<typename X>
    X fail() {
      assert(false);
    }
    
    template<typename T>
    struct TrampolineNode;

    template<typename T>
    using Trampoline = std::shared_ptr<TrampolineNode<T>>;

    template<typename T>
    struct TrampolineNode {
      virtual ~TrampolineNode() { }
      virtual bool is_return() = 0;
      virtual const T& from_return() = 0;
      virtual const std::function<Trampoline<T>()>& from_tc() = 0;
    };

    template<typename T>
    struct ReturnNode : TrampolineNode<T> {
      T t;
      ReturnNode(T&& t) : t(std::move(t)) { }
      ReturnNode(const T& t) : t(t) { }
      bool is_return() override { return true; }
      const T& from_return() override { return t; }
      const std::function<Trampoline<T>()>& from_tc() override { assert(false); }
    };

    template<typename T>
    struct TCNode : TrampolineNode<T> {
      std::function<Trampoline<T>()> f;
      TCNode(std::function<Trampoline<T>()>&& f) : f(std::move(f)) { }
      bool is_return() override { return false; }
      const T& from_return() override { assert(false); }
      const std::function<Trampoline<T>()>& from_tc() override { return f; }
    };

    template<typename T>
    struct TCSP {
      Trampoline<std::shared_ptr<T>> t;
      TCSP(const std::shared_ptr<T>& sp) : t(std::make_shared<ReturnNode<std::shared_ptr<T>>>(sp)) { }
      TCSP(std::shared_ptr<T>&& sp) : t(std::make_shared<ReturnNode<std::shared_ptr<T>>>(std::move(sp))) { }
      TCSP(std::function<Trampoline<std::shared_ptr<T>>()>&& f) : t(std::make_shared<TCNode<std::shared_ptr<T>>>(std::move(f))) { }
    };

    template<typename Ret, typename... X>
    TCSP<Ret> FuncApp(
      const std::shared_ptr<std::function<TCSP<Ret>(const std::shared_ptr<X>&...)>>& f,
      const std::shared_ptr<X>&... x) {
      return TailCall([=](const std::function<TCSP<Ret>(const std::shared_ptr<X>&...)>& func) { return func(x...); }, f);
    }

    template<typename T>
    TCSP<T> ToTCSP(std::shared_ptr<T>&& sp) {
      return TCSP<T>(std::move(sp));
    }
    template<typename T>
    TCSP<T> ToTCSP(const std::shared_ptr<T>& sp) {
      return TCSP<T>(sp);
    }
    template<typename T>
    TCSP<T> ToTCSP(TCSP<T>&& tcsp) {
      return tcsp;
    }
    template<typename T>
    TCSP<T> ToTCSP(const TCSP<T>& tcsp) {
      return tcsp;
    }

    template<typename F, typename ...X>
    auto TailCall(F&& f, const std::shared_ptr<X>&... x) {
      using ret_type = decltype(ToTCSP(f((*x)...)));
      return ret_type([=, f=std::forward<F>(f)](){ return ToTCSP(f((*x)...)).t; });
    }
    template<typename F>
    auto bindZombieTC(F&& f) {
      auto tcsp = ToTCSP(f());
      while (!tcsp.t->is_return()) {
        tcsp.t = tcsp.t->from_tc()();
        record();
      }
      return tcsp.t->from_return();
    }
    """
  }
  def handle_constructor(x: TypeDecl): String = {
    ""
  }
  def codegen_binds_raw(captured: Set[String], x: Seq[String], y: Seq[String] => Value): Value = {
    val binds = x.map(x => (x, freshName()))
    Value.Stmts(s"""
    ${binds.map((x, n) => s"auto ${n} = *${x};").mkString("\n")}
    ${y(binds.map((x, n) => s"${n}")).toStmts}
    """)
  }
  def val_wrapper_raw(t: String, v: String): String = {
    s"""std::make_shared<${t}>(${v})"""
  }
}

class CodeGenEnv(p: Program, be: BackEnd) {
  val BE = be
  val tyck: TyckEnv = tyck_program(p)
  val constructor_adt_name_map: mutable.Map[String, String] = mutable.Map[String, String]()
  p.tds.map(td => td.cons.map(cb => constructor_adt_name_map.put(cb.name, td.name)))
  val constructor_position_map: mutable.Map[String, Int] =
    mutable.Map[String, Int]()
  p.tds.map(td =>
    td.cons.zipWithIndex.map((cb, idx) =>
      constructor_position_map.put(cb.name, idx)
    )
  )
  val adt_constructor_count_map: mutable.Map[String, Int] = mutable.Map[String, Int]()
  p.tds.map(td =>
    adt_constructor_count_map.put(td.name, td.cons.length))
  val global_funcs: Set[String] = {
    p.vds.map(vd => vd.x).toSet
  }
  var macros = Seq[String]()
  val type_hc = mutable.Map[String, String]()

  def finish(x: String): String = {
    macros.mkString("\n") + "\n" + x
  }
}

def codegen_args(bindings: Seq[String], env: CodeGenEnv): String = {
  bracket(
    bindings
      .map(y => s"const ${env.BE.type_wrapper(codegen_type(tyck_expr(Expr.Var(y), env.tyck), env))}& ${y}")
      .mkString(", ")
  )
}

def codegen_template_header_from_name(n: String, env: CodeGenEnv) = {
  env.tyck.gvar_map(n) match {
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
          env.BE.return_type_wrapper(
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

def to_simp_cons(pat: Pat): (String, Seq[String]) = {
  pat match
    case Pat.Cons(name, xs) => (name, xs.map(simple_pat_to_name))
}

def get_adt_name(t: Type) = {
  resolve(t) match
    case Type.TyCons(n)              => n
    case Type.App(Type.TyCons(n), _) => n
}

def codegen_expr(x: Expr, env: CodeGenEnv, is_tail: Boolean): Value = {
  val recurse = x => codegen_expr(x, env, false)
  val recurse_tail = x => codegen_expr(x, env, true)
  val recurse_expr_tail = (x: Expr) => recurse_tail(x).toExpr
  val recurse_expr = (x: Expr) => recurse(x).toExpr
  val recurse_stmts = (x: Expr) => recurse(x).toStmts
  val recurse_stmts_tail = (x: Expr) => recurse_tail(x).toStmts
  
  def codegen_binds(captured: Set[String], x: Seq[Value], y: Seq[String] => Value): Value = {
    env.BE.codegen_binds(is_tail, captured, x, y)
  }
  def codegen_bind(captured: Set[String], x: Value, y: String => Value): Value = {
    codegen_binds(captured, Seq(x), x_ => y(x_(0)))
  }

  x match {
    case Expr.Var(name) => Value.Expr(name)
    case Expr.Match(matched, cases) => {
      val matched_type = env.tyck.expr_map(matched)
      val transformed_cases = cases
        .map(x => (to_simp_cons(x(0)), x(1)))
        .sortBy(x => env.constructor_position_map(x(0)(0)))
      val adt_name = env.constructor_adt_name_map(to_simp_cons(cases(0)(0))(0))
      assert(env.adt_constructor_count_map(adt_name) == cases.length)
      transformed_cases.zipWithIndex.map((x, i) =>
        assert(env.constructor_position_map(x(0)(0)) == i)
      )
      Value.Expr(
        s"${get_adt_name(matched_type)}Match(" +
          s"${recurse_expr(matched)}, ${transformed_cases
              .map((lhs, rhs) => {
                val capture = free_vars(rhs).diff(lhs(1).toSet)
                lambda(capture.mkString(", "), lhs(1).map(n => "const auto& " ++ n).mkString(", "), recurse_stmts_tail(rhs))
              })
              .mkString(", ")})"
      )
    }
    case Expr.App(Expr.GVar(f), xs) => {
      if (env.global_funcs.contains(f)) {
        Value.Expr(f + bracket(xs.map(recurse_expr).mkString(", ")))
      } else {
        Value.Expr(
          s"FuncApp(${(Seq(f) ++ xs.map(recurse_expr)).mkString(", ")})"
        )
      }
    }
    case Expr.App(f, xs) => {
      Value.Expr(
        s"FuncApp(${(Seq(recurse_expr(f)) ++ xs.map(recurse_expr)).mkString(", ")})"
      )
    }
    case Expr.Abs(bindings, body) => {
      val captured = free_vars(x)
      env.BE.val_wrapper(
        codegen_type(
          env.tyck.expr_map(x),
          env
        ),
        lambda(captured.mkString(", "), bindings
            .map(b =>
              named_cref_wrapper(
                env.BE.type_wrapper(codegen_type(env.tyck.var_map(b), env)),
                b
              )
            )
            .mkString(", "), recurse_tail(body).toStmts)
      )
    }
    case Expr.Cons(name, xs) => {
      Value.Expr((resolve(env.tyck.expr_map(x)) match {
        case Type.App(f, xs) =>
          name + s"""<${xs.map(codegen_type(_, env)).mkString(", ")}>"""
        case Type.TyCons(_) => name
      }) + bracket(xs.map(recurse_expr).mkString(", ")))
    }
    case Expr.Let(bs, body) => {
      Value.Stmts(s"""
        ${bs.map((n, v) => s"auto ${n} = ${recurse_expr(v)};").mkString("\n")} 
        ${recurse_stmts_tail(body)}
      """)
    }
    case Expr.LitInt(x) => {
      env.BE.val_wrapper("int64_t", x.toString)
    }
    case Expr.LitBool(x) => {
      env.BE.val_wrapper("bool", if (x) "true" else "false")
    }
    case Expr.If(i, t, e) =>
      codegen_bind(
        free_vars(t).union(free_vars(e)),
        recurse(i),
        i_ => Value.Expr(s"(${i_} ? ${recurse_tail(t)} : ${recurse_tail(e)})")
      )
    case Expr.Prim(l, op, r) => {
      codegen_binds(
        Set(),
        Seq(recurse(l), recurse(r)),
        xs => {
          env.BE.val_wrapper(
            codegen_type(env.tyck.expr_map(x), env),
            codegen_primop(xs(0), op, xs(1))
          )
        }
      )
    }
    case Expr.PrimCPS(l, op, r, k) => {
      codegen_binds(
        Set(),
        Seq(recurse(l), recurse(r), recurse(k)),
        xs => {
          val t = resolve(env.tyck.expr_map(k)) match {
            case Type.Func(l, r) => {
              assert(l.length == 1)
              l(0)
            }
          }
          val v = env.BE.val_wrapper(
            codegen_type(t, env),
            codegen_primop(xs(0), op, xs(1)))
          Value.Expr(s"${xs(2)}(${v})")
        }
      )
    }
    case Expr.Fail() => {
      Value.Expr(s"fail<${env.BE.type_wrapper(codegen_type(env.tyck.expr_map(x), env))}>()")
    }
  }
}

def cref_wrapper(t: String): String = {
  s"const ${t}&"
}

def named_cref_wrapper(tn: (String, String)): String = {
  s"const ${tn(0)}& ${tn(1)}"
}

def codegen_type_raw(x: Type, env: CodeGenEnv): String = {
  val recurse = y => codegen_type(y, env)
  resolve(x) match {
    case Type.Var(name, _) => name
    case Type.TyCons(name) => name
    case Type.Func(args, ret) =>
      s"std::function<${env.BE.return_type_wrapper(recurse(ret))}(${args
          .map(x => cref_wrapper(env.BE.type_wrapper(recurse(x))))
          .mkString(", ")})>"
    case Type.App(Type.TyCons(name), xs) =>
      s"${name}<${xs.map(recurse).mkString(", ")}>"
    case Type.Prim(PrimType.INT)  => "int64_t"
    case Type.Prim(PrimType.BOOL) => "bool"
  }
}

def codegen_type(x: Type, env: CodeGenEnv): String = {
  val ret = codegen_type_raw(x, env)
  env.type_hc.get(ret) match {
    case Some(x) => x
    case None =>
      if (ret.length < 20) {
        ret
      } else {
        val fn = freshName()
        env.macros = env.macros :+ s"#define ${fn} ${ret}"
        env.type_hc.put(ret, fn)
        fn
      }
  }
}

def codegen_types(x: Seq[Type], env: CodeGenEnv): Seq[String] = {
  x.map(t => codegen_type(t, env))
}

def codegen_types_wrapped(x: Seq[Type], env: CodeGenEnv): Seq[String] = {
  x.map(t => env.BE.type_wrapper(codegen_type(t, env)))
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

def typename(x: String): String = {
  "typename " + x
}

def codegen_constructor_type(x: TypeDecl): String = {
  x.name + (if (x.xs.isEmpty) { "" }
            else {
              s"<${x.xs.mkString(", ")}>"
            })
}

def codegen_match(x: TypeDecl, env: CodeGenEnv): String = {
  val matched_name = freshName()
  val matcher_name = x.cons.map(_ => freshName())
  val matcher_type = x.cons.map(_ => freshName())
  val header = s"""
    ${codegen_template_header(x.xs)}
    auto ${x.name}Match(${(Seq(
      named_cref_wrapper(
        env.BE.type_wrapper(codegen_constructor_type(x)),
        matched_name
      )
    ) ++ matcher_name.map(F => "const auto& " + F)).mkString(", ")})
  """
  header + cbracket(
    env.BE.codegen_bind(
      true,
      matcher_name.toSet,
      Value.Expr(matched_name),
      matched =>
        Value.Stmts(
          x.cons.zipWithIndex
            .map((cb, i) =>
              s"""if (${matched}.var.index() == ${i}) {
                ${val args_name = cb.args.map(_ => freshName());
                cb.args.zipWithIndex
                  .map((a, ii) =>
                    s"auto ${args_name(ii)} = std::get<${ii}>(std::get<${i}>(${matched}.var));"
                  )
                  .mkString("\n") ++
                  s"return ${matcher_name(i)}(${args_name.mkString(", ")});" }
              }"""
            )
            .mkString("\n")
            +
              "assert(false);"
        )
    ).toStmts
  )
}

def codegen_td(x: TypeDecl, env: CodeGenEnv): String = {
  s"""
  ${codegen_template_header(x.xs)}
  struct ${x.name} { ${variant_td(x, env)} var; };
  ${codegen_constructors(x, env)}
  ${codegen_match(x, env)}
  """
}

def variant_td(x: TypeDecl, env: CodeGenEnv): String = {
  s"std::variant<${x.cons.map(cb => tuplify(codegen_types_wrapped(cb.args, env))).mkString(", ")}>"
}

def codegen_constructors(x: TypeDecl, env: CodeGenEnv): String = {
  x.cons.zipWithIndex
    .map((cb, idx) => {
      val ret_type_unwrapped = codegen_constructor_type(x)
      val ret_type = env.BE.type_wrapper(ret_type_unwrapped)
      val name = cb.name
      val type_with_name = cb.args.map(ty => (ty, freshName()))
      val args = bracket(
        type_with_name
          .map((ty, n) =>
            named_cref_wrapper(env.BE.type_wrapper(codegen_type(ty, env)), n)
          )
          .mkString(", ")
      )
      s"""${codegen_template_header(x.xs)} ${ret_type} ${name}${args} {
       ${env.BE
          .val_wrapper(
            ret_type_unwrapped,
            ret_type_unwrapped + cbracket(s"""
            .var=${variant_td(
                x,
                env
              )}{std::in_place_index<${idx}>, ${type_with_name
                .map(_(1))
                .mkString(", ")}}
          """)
          )
          .toStmts}
    }"""
    })
    .mkString("\n") + env.BE.handle_constructor(x)
}

def codegen_vd(x: ValueDecl, env: CodeGenEnv): String = {
  if (x.x == "main") {
    s"int main() { bindZombieTC([](){ return ${codegen_expr(x.b, env, true)};}); }"
  } else {
    (x.b match {
      case Expr.Abs(bindings, body) => {
        codegen_template_header_from_name(x.x, env) +
          env.BE.return_type_wrapper(
            codegen_type(
              env.tyck.expr_map.get(body).get,
              env
            )
          ) + " " + x.x + codegen_args(bindings, env) +
          s"""{ 
          ${codegen_expr(body, env, true).toStmts}
        }"""
      }
    })
  }
}

def codegen(x: Program, backend: String, limit: Long, log_path: String): String = {
  val BE = if (backend == "baseline") { 
    NoZombieBackEnd()
   } else if (backend == "zombie") { 
    ZombieBackEnd(limit=limit)
  } else {
    assert(false)
  }

  val env = CodeGenEnv(x, BE)
  env.finish(
  s"""
  #define log_path "${log_path}"
  #include "override.h"

  #include <memory>
  #include <variant>
  #include <functional>
  #include <cassert>
  """ ++
    BE.header ++
    x.tds.map(codegen_td_fwd(_, env)).mkString("\n") +
    x.tds.map(codegen_td(_, env)).mkString("\n") +
    x.vds.map(codegen_vd_fwd(_, env)).mkString("\n") +
    x.vds.map(codegen_vd(_, env)).mkString("\n"))
}

def run_ok(x: ProcessBuilder) = {
  assert(x.! == 0)
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
  Process("clang-format --style='{ColumnLimit: 200}' -i output.cpp").!
  //Process("cat output.cpp").!
  println("compiling...")
  run_ok(Process("g++ -g -O3 -std=c++20 -o output output.cpp -lmimalloc"))
  //Process("g++ -g -std=c++20 -o output output.cpp -lmimalloc").!
  Process("cloc output.cpp").!
}