package zombie.tvirus.codegen
import org.antlr.v4.runtime.CharStreams
import java.io.FileWriter
import java.io.IOException
import zombie.tvirus.parser.*
import zombie.tvirus.prettier.*
import java.nio.file.{FileSystems, Files}
import collection.JavaConverters.asScalaIteratorConverter
import collection.mutable

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
    case v @ Type.Var(_, _) =>
      if (env.tcons_decl.contains(v.name)) { Type.TyCons(v.name) }
      else { v }
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

def free_vars(x: Expr): Set[String] = {
  val recurse = x => free_vars(x)
  x match
    case Expr.Var(n) => Set(n)
    case Expr.App(f, x) =>
      recurse(f).union(
        x.map(recurse).foldLeft(Set[String]())((l, r) => l.union(r))
      )
    case Expr.PrimCPS(l, op, r, k) =>
      recurse(l).union(recurse(r)).union(recurse(k))
    case Expr.Abs(l, r) => recurse(r).diff(Set(l: _*))
    case Expr.Fail() | Expr.GVar(_) | Expr.LitInt(_) => Set()
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
  // println(show(pp(x)))
  val tyck = TyckEnv(x)
  // for ((k, v) <- tyck.var_map) {
  //  println((k, pp_type(v)))
  // }
  val cpp_code =
    codegen(x, backend = backend, limit = limit, log_path = log_path)
  compile(cpp_code)
}
