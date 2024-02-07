package zombie.tvirus.codegen
import org.antlr.v4.runtime.CharStreams
import java.io.FileWriter
import java.io.IOException
import zombie.tvirus.parser.*
import zombie.tvirus.prettier.*
import java.nio.file.{FileSystems, Files}
import collection.JavaConverters.asScalaIteratorConverter
import collection.mutable
import cats.data.ZipSeq
import scala.sys.process._
import zombie.tvirus.prettier.Doc.bracketed
import cats.Monad

class Watcher {
  var progress = true
  def make_progress() = {
    progress = true
  }

  def progress_made() = {
    val ret = progress
    progress = false
    ret
  }
}

def hoas(f: (Expr => Expr)): Expr = {
  val x_ = freshName()
  Expr.Abs(Seq(x_), f(Expr.Var(x_)))
}

def is_var(t: Type) = {
  t match {
    case Type.Var(_, _) => true
    case _              => false
  }
}

def consExpr(e: Expr, env: ConsEnv): Expr = {
  val recurse = x => consExpr(x, env)
  e match
    case Expr.Prim(left, op, right) =>
      Expr.Prim(recurse(left), op, recurse(right))
    case Expr.App(f, xs) =>
      f match
        case Expr.Var(name) if env.cons_decl.contains(name) =>
          Expr.Cons(name, xs.map(recurse))
        case _ => Expr.App(recurse(f), xs.map(recurse))
    case Expr.Abs(xs, b) => Expr.Abs(xs, recurse(b))
    case Expr.Let(xs, b) =>
      Expr.Let(xs.map((l, r) => (l, recurse(r))), recurse(b))
    case Expr.Match(x, bs) =>
      Expr.Match(
        recurse(x),
        bs.map(b => (b(0), recurse(b(1))))
      )
    case Expr.Var(n) =>
      if (env.funcs_decl.contains(n)) { Expr.GVar(n) }
      else { Expr.Var(n) }
    case Expr.LitInt(x)   => Expr.LitInt(x)
    case Expr.LitBool(x)  => e
    case Expr.If(i, t, e) => Expr.If(recurse(i), recurse(t), recurse(e))
}

def consType(x: Type, env: ConsEnv): Type = {
  val recurse = x => consType(x, env)
  resolve(x) match {
    case Type.Var(name, _) =>
      if (env.tcons_decl.contains(name)) { Type.TyCons(name) }
      else { x }
    case Type.App(f, xs) => Type.App(recurse(f), xs.map(recurse))
    case Type.Prim(t)    => Type.Prim(t)
  }
}
class ConsEnv(p: Program) {
  val cons_decl: Set[String] = p.tds.foldLeft(Set.empty)((consDecls, td) =>
    consDecls ++ td.cons.map(_.name)
  )
  val tcons_decl: Set[String] = p.tds.map(_.name).toSet
  val funcs_decl: Set[String] = p.vds.map(_.x).toSet
}
def cons(p: Program): Program = {
  val env = ConsEnv(p)
  Program(
    p.tds.map(td =>
      TypeDecl(
        td.name,
        td.xs,
        td.cons.map(cb => CBind(cb.name, cb.args.map(t => consType(t, env))))
      )
    ),
    p.vds.map(vd => vd.copy(b = consExpr(vd.b, env)))
  )
}

def let_(v: Expr, b: Expr => Expr): Expr = {
  val fresh = freshName()
  Expr.Let(Seq((fresh, v)), b(Expr.Var(fresh)))
}

def check_dup(p: Program): Unit = {
  val seen = mutable.Set[String]()
  def register(x: String): Unit = {
    if (seen.contains(x)) {
      println(s"name ${x} is used to mean different things")
      assert(false)
    } else {
      seen.add(x)
    }
  }
  p.tds.map(td => {
    register(td.name)
    td.cons.map(cb => register(cb.name))
  })
  p.vds.map(vd => register(vd.x))
}

def unions(x: Seq[Set[String]]): Set[String] = {
  x.foldLeft(Set[String]())((l, r) => l.union(r))
}

def pattern_vars(x: Pat): Set[String] = {
  val recurse = x => pattern_vars(x)
  x match
    case Pat.Var(n) => Set(n)
    case Pat.Cons(n, xs) => unions(xs.map(recurse))
}

def free_vars(x: Expr): Set[String] = {
  val recurse = x => free_vars(x)
  x match
    case Expr.Var(n) => Set(n)
    case Expr.App(f, x) =>
      recurse(f).union(unions(x.map(recurse)))
    case Expr.PrimCPS(l, op, r, k) =>
      recurse(l).union(recurse(r)).union(recurse(k))
    case Expr.Abs(l, r) => recurse(r).diff(Set(l: _*))
    case Expr.Fail() | Expr.GVar(_) | Expr.LitInt(_) => Set()
    case Expr.If(i, t, e) => recurse(i).union(recurse(t)).union(recurse(e))
    case Expr.Cons(_, x) => unions(x.map(recurse))
    case Expr.Match(x, cases) => recurse(x).union(unions(cases.map((lhs, rhs) => recurse(rhs).diff(pattern_vars(lhs)))))
  }

def codegen_ocaml_type(x: Type, env: OCamlEnv): String = {
  val recurse = x => codegen_ocaml_type(x, env)
  resolve(x) match
    case Type.Var(n, _) => "'" ++ n
    case Type.TyCons(n) => {
      if (env.untouched_td.contains(n)) {
        codegen_ocaml_td(env.untouched_td(n), env)
      }
      n.toLowerCase()
    }
    case Type.App(l, r) => r.map(recurse).mkString(" ") ++ " " ++ recurse(l)
} 

def codegen_cb(cb: CBind, env: OCamlEnv): String = {
  if (cb.args.length == 0) {
    cb.name
  } else {
    s"${cb.name} of ${cb.args.map(x => codegen_ocaml_type(x, env)).mkString(" * ")}"
  }
}

def codegen_ocaml_td(td: TypeDecl, env: OCamlEnv): Unit = {
  if (env.untouched_td.contains(td.name)) {
    env.untouched_td.remove(td.name)
    val str = s"type ${td.xs.map(x => "'" ++ x).mkString(" ")} ${td.name.toLowerCase()} = ${td.cons.map(x => codegen_cb(x, env)).mkString("|")};;\n"
    env.result += str
  }
}

def codegen_ocaml_binop(op: PrimOp): String = {
  op match
    case PrimOp.MINUS => "-"
    case PrimOp.EQ => "=="
    case PrimOp.GT => ">"
    case PrimOp.LT => "<"
    case PrimOp.ADD => "+"
}

def codegen_ocaml_pattern(x: Pat): String = {
  val recurse = x => codegen_ocaml_pattern(x)
  x match
    case Pat.Wildcard => "_"
    case Pat.Cons(n, xs) => {
      if (xs.length == 0) {
        n
      } else {
        n ++ bracket(xs.map(recurse).mkString(", "))
      }
    }
    case Pat.Var(n) => n
}

def codegen_ocaml_expr(x: Expr, env: OCamlEnv): String = {
  val recurse = x => codegen_ocaml_expr(x, env)
  x match
    case Expr.Var(n) => n
    case Expr.GVar(n) => {
      if (env.untouched_vd.contains(n)) {
        codegen_ocaml_vd(env.untouched_vd(n), env)
      }
      n
    }
    case Expr.App(f, xs) => recurse(f) ++ bracket(xs.map(recurse).mkString(", "))
    case Expr.LitInt(i) => i.toString()
    case Expr.Abs(lhs, rhs) => s"(fun (${lhs.mkString(", ")}) -> ${recurse(rhs)})"
    case Expr.PrimCPS(l, op, r, k) => recurse(k) ++ bracket(recurse(Expr.Prim(l, op, r)))
    case Expr.If(i, t, e) => s"(if (${recurse(i)}) then (${recurse(t)}) else (${recurse(e)}))"
    case Expr.Cons(n, args) => {
      if (args.length == 0) {
        n
      } else {
        n ++ bracket(args.map(recurse).mkString(", "))
      }
    }
    case Expr.Prim(l, op, r) => s"(${recurse(l)} ${codegen_ocaml_binop(op)} ${recurse(r)})"
    case Expr.Match(x, cases) => s"(match ${recurse(x)} with ${cases.map((lhs, rhs) => codegen_ocaml_pattern(lhs) + "->" + recurse(rhs)).mkString("|")})"
}

def codegen_ocaml_vd(x: ValueDecl, env: OCamlEnv): Unit = {
  if (env.untouched_vd.contains(x.x)) {
    env.untouched_vd.remove(x.x)
    val str = s"let rec ${x.x} = ${codegen_ocaml_expr(x.b, env)};;\n"
    env.result += str
  }
}

class OCamlEnv(p: Program) {
  val untouched_vd: mutable.Map[String, ValueDecl] = mutable.Map[String, ValueDecl](p.vds.map(x => x.x -> x):_*)
  val untouched_td: mutable.Map[String, TypeDecl] = mutable.Map[String, TypeDecl](p.tds.map(x => x.name -> x):_*)
  var result: String = ""
}

def codegen_ocaml(x: Program): String = {
  val env = OCamlEnv(x)
  x.tds.map(td => codegen_ocaml_td(td, env))
  x.vds.map(vd => codegen_ocaml_vd(vd, env))
  env.result
}

def compile_ocaml(x: String) = {
  Process("rm output.ml")
  val fileWriter = new FileWriter("output.ml")
  fileWriter.write(x)
  fileWriter.close()
  Process("ocamlformat -i output.ml").!
  Process("cat output.ml").!
  Process("time ocaml output.ml").!
}

@main def main(
    program: String,
    backend: String,
    limit: Long,
    log_path: String
): Unit = {
  val libdir = FileSystems.getDefault.getPath("library")
  val libcontents = Files
    .list(libdir)
    .iterator()
    .asScala
    .filter(Files.isRegularFile(_))
    .map((f) => os.read(os.Path(f, os.pwd)))
    .reduce((a, b) => a + b)
  val src = libcontents + os.read(os.Path(s"example/${program}.tv", os.pwd))

  var x = drive(CharStreams.fromString(src))
  check_dup(x)
  x = refresh(dce(cons(x)))
  println(show(pp(x)))
  x = reify_global_funcs(x)
  // println(show(pp(x)))
  x = simpl(unnest_match(x))
  println("unnest ok!!!")
  // println(pp(x).toString.length)
  // println(show(pp(x)))
  x = cps(x)
  println("cps done!!!")
  //println(show(pp(x)))
  val tyck = TyckEnv(x)
  for (vd <- x.vds) {
    println(vd.x + ": " + show(pp_type(tyck.gvar_map(vd.x))))
  }
  // for ((k, v) <- tyck.var_map) {
  //  println((k, pp_type(v)))
  // }
  //val ocaml_code = codegen_ocaml(x)
  //compile_ocaml(ocaml_code)
  val cpp_code =
    codegen(x, backend = backend, limit = limit, log_path = log_path)
  compile(cpp_code)
}
