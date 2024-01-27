package zombie.tvirus.codegen
import zombie.tvirus.parser.*
import collection.mutable

def td_in(p: Program, td: TypeDecl): Boolean = {
  member(td.name, p.tds.map(td => td.name))
}

def add_td(p: Program, td: TypeDecl): Program = {
  if (td_in(p, td)) {
    p
  } else {
    Program(p.tds :+ td, p.vds)
  }
}

def get_td(p: Program, name: String): TypeDecl = {
  p.tds.filter(td => td.name == name)(0)
}

def vd_in(p: Program, vd: ValueDecl): Boolean = {
  member(vd.x, p.vds.map(vd => vd.x))
}

def add_vd(p: Program, vd: ValueDecl): Program = {
  if (vd_in(p, vd)) {
    p
  } else {
    Program(p.tds, p.vds :+ vd)
  }
}

def get_vd(p: Program, x: String): Option[ValueDecl] = {
  val res = p.vds.filter(vd => vd.x == x)
  if (res.length > 0) {
    Some(res(0))
  } else {
    None
  }
}

def make_cons_map(tds: Seq[TypeDecl]): Map[String, String] = {
  def iter(i: Int, acc: Map[String, String]): Map[String, String] = {
    if (i == tds.length) {
      acc
    } else {
      val conses = tds(i).cons
      val name = tds(i).name
      def iter_conses(j: Int, acc: Map[String, String]): Map[String, String] = {
        if (j == conses.length) {
          acc
        } else {
          val cons = conses(j)
          val p = cons.name -> name
          iter_conses(j + 1, acc + p)
        }
      }
      iter(i + 1, iter_conses(0, acc))
    }
  }
  iter(0, Map())
}

def collect_multiple(
    es: Seq[Expr],
    p: Program,
    cm: Map[String, String],
    acc: Program
): Program = {
  def iter(i: Int, acc: Program): Program = {
    if (i == es.length) {
      acc
    } else {
      val e = es(i)
      iter(i + 1, collect_decls(e, p, cm, acc))
    }
  }
  iter(0, acc)
}

def collect_decls(
    e: Expr,
    p: Program,
    cm: Map[String, String],
    acc: Program
): Program = {
  e match {
    case Expr.Prim(l, op, r) =>
      collect_multiple(Array(l, r), p, cm, acc)
    case Expr.Var(name) => {
      val r = get_vd(p, name)
      r match {
        case Some(vd) =>
          if (vd_in(acc, vd)) { acc }
          else {
            collect_decls(vd.b, p, cm, add_vd(acc, vd))
          }
        case None => acc
      }
    }
    case Expr.LitInt(x)  => acc
    case Expr.LitBool(x) => acc
    case Expr.App(f, xs) => collect_multiple(xs :+ f, p, cm, acc)
    case Expr.Abs(xs, b) => collect_decls(b, p, cm, acc)
    case Expr.Let(xs, b) => collect_multiple(xs.map((name, x) => x) :+ b, p, cm, acc)
    case Expr.Match(x, bs) =>
      collect_multiple(
        bs.map(pattern => {
          pattern match {
            case (pat, expr) => expr
          }
        }),
        p,
        cm,
        acc
      )
    case Expr.Cons(name, args) =>
      collect_multiple(
        args,
        p,
        cm,
        add_td(acc, get_td(p, cm(name)))
      )
    case Expr.If(cond, conseq, alt) =>
      collect_multiple(Array(cond, conseq, alt), p, cm, acc)
  }
}

def dce(p: Program): Program = {
  val tds = p.tds
  val cm = make_cons_map(tds)
  collect_decls(
    Expr.Var("main"),
    p,
    cm,
    Program(Array[TypeDecl](), Array[ValueDecl]())
  )
}

def show_program(p: Program): Program = {
  println("code:")
  println(p.vds.map(x => x.x))
  println(show(pp(p)))
  p
}
