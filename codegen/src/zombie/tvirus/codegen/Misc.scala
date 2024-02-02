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

def consExpr(e: Expr, consDecls: Set[String]): Expr = {
  val recurse = x => consExpr(x, consDecls)
  e match
    case Expr.Prim(left, op, right) =>
      Expr.Prim(recurse(left), op, recurse(right))
    case Expr.App(f, xs) =>
      f match
        case Expr.Var(name) if consDecls.contains(name) =>
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
    case Expr.Var(n)      => Expr.Var(n)
    case Expr.LitInt(x)   => Expr.LitInt(x)
    case Expr.LitBool(x)  => e
    case Expr.If(i, t, e) => Expr.If(recurse(i), recurse(t), recurse(e))
}

def consType(x: Type, decls: Set[String]): Type = {
  val recurse = x => consType(x, decls)
  resolve(x) match {
    case v @ Type.Var(_, _) =>
      if (decls.contains(v.name)) { Type.TyCons(v.name) }
      else { v }
    case Type.App(f, xs) => Type.App(recurse(f), xs.map(recurse))
    case Type.Prim(t)    => Type.Prim(t)
  }
}
def cons(p: Program): Program = {
  val consDecls: Set[String] = p.tds.foldLeft(Set.empty)((consDecls, td) =>
    consDecls ++ td.cons.map(_.name)
  )
  val tconsDecls: Set[String] = p.tds.map(_.name).toSet
  Program(
    p.tds.map(td =>
      TypeDecl(
        td.name,
        td.xs,
        td.cons.map(cb =>
          CBind(cb.name, cb.args.map(t => consType(t, tconsDecls)))
        )
      )
    ),
    p.vds.map(vd => vd.copy(b = consExpr(vd.b, consDecls)))
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

@main def main(program: String, backend: String, log_path: String): Unit = {
  val libdir = FileSystems.getDefault.getPath("library")
  val libcontents = Files.list(libdir).iterator().asScala.filter(Files.isRegularFile(_)).map((f) => os.read(os.Path(f, os.pwd))).reduce((a, b) => a + b)
  val src = libcontents + os.read(os.Path(s"example/${program}.tv", os.pwd))

  var x = drive(CharStreams.fromString(src))
  check_dup(x)
  x = dce(cons(x))
  println(show(pp(x)))
  x = reify_global_funcs(refresh(x))
  println(show(pp(x)))
  x = simpl(unnest_match(x))
  println("unnest ok!!!")
  println(show(pp(x)))
  x = cps(x)
  println("cps done!!!")
  println(show(pp(x)))
  val tyck = TyckEnv(x)
  // for ((k, v) <- tyck.var_map) {
  //  println((k, pp_type(v)))
  // }
  val cpp_code = codegen(x, backend=backend, log_path=log_path)
  compile(cpp_code)
}
