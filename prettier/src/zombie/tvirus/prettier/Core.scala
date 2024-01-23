package zombie.tvirus.prettier

import cats.kernel.Order
import cats.syntax.all.*
import cats.Eval

trait Cost[T] extends Order[T]:
  val nl: T
  def text(c: Int, l: Int): T
  
  extension (x: T)
    def +(y: T): T

object Cost:
  def apply[T](using c: Cost[T]) = c

case class Measure[C: Cost](last: Int, cost: C, doc: Doc) {
  def concat(other: Measure[C]): Measure[C] =
    Measure(other.last, cost + other.cost, doc <> other.doc)

  def nested(n: Int): Measure[C] =
    Measure(last, cost, Doc.Nest(n, doc))

  def aligned: Measure[C] =
    Measure(last, cost, Doc.Align(doc))
}

given [C: Cost]: Order[Measure[C]] with
  def compare(x: Measure[C], y: Measure[C]): Int = {
    if (x.last < y.last) {
      -1
    } else if (x.last > y.last) {
      1
    } else {
      Cost[C].compare(x.cost, y.cost)
    }
  }

extension [C: Cost](using M: Order[Measure[C]])(ls: List[Measure[C]])
  def concatMeasures(rs: List[Measure[C]]): List[Measure[C]] = (ls, rs) match
    case (Nil, Nil) => Nil
    case (Nil, _)   => rs
    case (_, Nil)   => ls
    case (hd0 :: tl0, hd1 :: tl1) =>
      if (M.lteqv(hd0, hd1)) {
        ls.concatMeasures(tl1)
      } else if (M.lteqv(hd1, hd0)) {
        tl0.concatMeasures(rs)
      } else if (hd0.last > hd1.last) {
        hd0 :: (tl0.concatMeasures(rs))
      } else {
        hd1 :: (ls.concatMeasures(tl1))
      }

enum MeasureSet[C](using Cost[C]):
  case Tainted(m: Eval[Measure[C]])
  case Set(l: List[Measure[C]])

  def tainted = this match
    case Tainted(m) => Tainted(m)
    case Set(l)     => Tainted(Eval.now(l.head))

  def lifted(f: Measure[C] => Measure[C]) = this match
    case Tainted(m) => Tainted(Eval.later(f(m.value)))
    case Set(l)     => Set(l.map(f))

  def concat(other: MeasureSet[C]) = (this, other) match
    case (_, Tainted(_))      => this
    case (Tainted(_), Set(_)) => other
    case (Set(ls), Set(rs))   => Set(ls.concatMeasures(rs))

// def memoize[I, O](f: I => O): I => O = new mutable.HashMap[I, O]() {
//   override def apply(key: I) = getOrElseUpdate(key, f(key))
// }

enum Doc:
  case Text(t: String)
  case NewLine
  case Concat(a: Doc, b: Doc)
  case Nest(n: Int, d: Doc)
  case Align(d: Doc)
  case Choice(a: Doc, b: Doc)

  def <>(o: Doc) = Concat(this, o)

  def layouted: String = {
    def aux(d: Doc, c: Int, i: Int): List[String] = d match
      case Text(t) => List(t)
      case NewLine => List("", " " * i)
      case Concat(a, b) => {
        val as = aux(a, c, i)
        val bs = if (as.length > 1) {
          aux(b, as.last.length(), i)
        } else {
          aux(b, c + as.last.length(), i)
        }
        as.init.appended(as.last ++ bs.head).appendedAll(bs.tail)
      }
      case Nest(n, ds) => aux(ds, c, i + n)
      case Align(ds)   => aux(ds, c, c)
    aux(this, 0, 0).mkString("\n")
  }

  def measured[C: Cost](c: Int, i: Int): Measure[C] = this match
    case Text(t) => Measure(c + t.length(), Cost[C].text(c, t.length()), this)
    case NewLine => Measure(i, Cost[C].nl + Cost[C].text(0, i), this)
    case Concat(a, b) => {
        val ma = a.measured(c, i)
        val mb = b.measured(ma.last, i)
        ma
    }
    case Nest(n, d) =>
    case Align(d) =>
    case Choice(a, b) =>
  

given Conversion[String, Doc] = Doc.Text.apply

// object Doc:
//   def <>
@main
def main() = {
  val v1 = "= func(" <> Doc.Nest(
    2,
    Doc.NewLine <> "pretty," <> Doc.NewLine <> "print"
  ) <> Doc.NewLine <> ")"
  val v2 = "a" <> Doc.Nest(42, Doc.Align("b" <> Doc.NewLine <> "c"))
  println(v2.layouted)
}
