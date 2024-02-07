package zombie.tvirus.prettier

import cats.Eval
import cats.kernel.Order
import cats.syntax.partialOrder.*

import com.dynatrace.hash4j.hashing.*

import scala.collection.mutable

private def memoize[I, O](f: I => O): I => O = new mutable.HashMap[I, O]() {
  override def apply(key: I) = getOrElseUpdate(key, f(key))
}

enum Doc:
  case Text(s: String)
  case Nl()
  case Nest(n: Int, d: Doc)
  case Align(d: Doc)
  case Concat(a: Doc, b: Doc)
  case Choice(a: Doc, b: Doc)

  private lazy val hashKey = {
    val stream = Hashing.wyhashFinal4().hashStream()
    this match
      case Text(s)    => stream.putString(s).getAsInt()
      case Nl()       => stream.putChar('\n').putInt(2).getAsInt()
      case Nest(n, d) => stream.putInt(n).putInt(d.hashCode()).getAsInt()
      case Align(d)   => stream.putInt(d.hashCode()).getAsInt()
      case Concat(a, b) =>
        stream.putInt(1).putInt(a.hashCode()).putInt(b.hashCode()).getAsInt()
      case Choice(a, b) =>
        stream.putInt(2).putInt(a.hashCode()).putInt(b.hashCode()).getAsInt()
  }

  override def hashCode(): Int = hashKey

  override def equals(x: Any): Boolean = {
    if (!x.isInstanceOf[Doc]) {
      false
    } else {
      (this, x) match
        case (Text(s1), Text(s2))             => s1.equals(s2)
        case (Nl(), Nl())                     => true
        case (Nest(n1, d1), Nest(n2, d2))     => n1 == n2 && d1.equals(d2)
        case (Align(d1), Align(d2))           => d1.equals(d2)
        case (Concat(a1, b1), Concat(a2, b2)) => a1.equals(a2) && b1.equals(b2)
        case (Choice(a1, b1), Choice(a2, b2)) => a1.equals(a2) && b1.equals(b2)
        case _                                => false
    }
  }

  private lazy val measureCache: mutable.HashMap[(Int, Int), Any] =
    mutable.HashMap.empty

  def measured[C: Cost](c: Int, i: Int) =
    measureCache
      .getOrElseUpdate((c, i), measure(this, c, i))
      .asInstanceOf[Measure[C]]

  private lazy val resolveCache: mutable.HashMap[(Int, Int), Any] =
    mutable.HashMap.empty

  def resolved[C: Cost](c: Int, i: Int) =
    resolveCache
      .getOrElseUpdate((c, i), resolve(this, c, i))
      .asInstanceOf[MeasureSet[C]]

given Conversion[String, Doc] = Doc.text

object Doc:
  val hashTable: mutable.WeakHashMap[Doc, Doc] = mutable.WeakHashMap()

  extension (d: Doc) def hashConsed = hashTable.getOrElseUpdate(d, d)

  def text(s: String) = Doc.Text(s).hashConsed
  def nl = Doc.Nl().hashConsed
  def nest(n: Int, d: Doc) = Doc.Nest(n, d).hashConsed
  def align(d: Doc) = Doc.Align(d).hashConsed
  def concat(a: Doc, b: Doc) = Doc.Concat(a, b).hashConsed
  def choice(a: Doc, b: Doc) = Doc.Choice(a, b).hashConsed

  extension (d: Doc)
    def <>(other: Doc) = concat(d, other)
    def <|>(other: Doc) = choice(d, other)
    def <+>(other: Doc) = d <> align(other)
    def <\>(other: Doc) = d <> nl <> other
    def printed(w: Int = 80) = {
      hashTable.clear()
      d.resolved(0, 0)(using Cost.sumOfSquared(w)).layouted.mkString("\n")
    }

  def sbreak = " " <|> Doc.nl

  def group(d: Doc) = d <|> flatten(d)

  def vcat(c: Iterable[Doc]) = c.reduceOption(_ <\> _).getOrElse(text(""))

  lazy val flatten: Doc => Doc = memoize { ds =>
    ds match
      case Text(_)      => ds
      case Nl()         => " "
      case Nest(n, d)   => flatten(d)
      case Align(d)     => flatten(d)
      case Concat(a, b) => flatten(a) <> flatten(b)
      case Choice(a, b) => flatten(a) <|> flatten(b)
  }

  def bracketed(
      d: Doc,
      o: String = "(",
      e: String = ")",
      space: Boolean = false
  ) = {
    val spc = if (space) { " " }
    else { "" }
    (o <> spc <> d <> spc <> e) <|> (o <> nest(2, nl <> d) <> nl <> e)
  }

def layout[C: Cost](ds: Doc, c: Int, i: Int): List[String] = ds match
  case Doc.Text(s)    => List(s)
  case Doc.Nl()       => List("", " " * i)
  case Doc.Nest(n, d) => layout(d, c, i + n)
  case Doc.Align(d)   => layout(d, c, c)
  case Doc.Concat(a, b) => {
    val la = layout(a, c, i)
    val lb = if (la.length > 1) {
      layout(b, la.head.length(), i)
    } else {
      layout(b, c + la.head.length(), i)
    }
    la.init :+ (la.last ++ lb.head) :++ lb.tail
  }
  case Doc.Choice(a, b) => throw new UnsupportedOperationException

trait Cost[T] extends Order[T]:
  val nl: T
  def text(c: Int, l: Int): T
  val cw: Int

  extension (a: T) def +(b: T): T

object Cost:
  def apply[T](using c: Cost[T]) = c

  def sumOfSquared(w: Int) = new Cost[(Int, Int)] {
    override def compare(x: (Int, Int), y: (Int, Int)): Int = {
      val c0 = Order[Int].compare(x(0), y(0))
      if (c0 == 0) {
        Order[Int].compare(x(1), y(1))
      } else {
        c0
      }
    }

    override val nl: (Int, Int) = (0, 1)

    override def text(c: Int, l: Int): (Int, Int) = {
      if (c + l > w) {
        val a = w.max(c) - w
        val b = c + l - w.max(c)
        (b * (2 * a + b), 0)
      } else {
        (0, 0)
      }
    }

    override val cw: Int = 100
    extension (a: (Int, Int))
      override def +(b: (Int, Int)): (Int, Int) = (a(0) + b(0), a(1) + b(1))
  }

case class Measure[C: Cost](last: Int, cost: C, doc: Doc):
  def concat(other: Measure[C]) =
    Measure(other.last, cost + other.cost, doc <> other.doc)

  def nested(n: Int) =
    Measure(last, cost, Doc.nest(n, doc))

  def aligned =
    Measure(last, cost, Doc.align(doc))

  def <=(other: Measure[C]): Boolean = last <= other.last && cost <= other.cost

def measure[C: Cost](ds: Doc, c: Int, i: Int): Measure[C] = ds match
  case Doc.Text(s) => Measure(c + s.length(), Cost[C].text(c, s.length()), ds)
  case Doc.Nl()    => Measure(i, Cost[C].nl + Cost[C].text(0, i), ds)
  case Doc.Nest(n, d) => d.measured(c, i).nested(n)
  case Doc.Align(d)   => d.measured(c, c).aligned
  case Doc.Concat(a, b) => {
    val ma = a.measured(c, i)
    val mb = b.measured(ma.last, i)
    ma concat mb
  }
  case Doc.Choice(a, b) => throw new UnsupportedOperationException

enum MeasureSet[C: Cost]:
  case Tainted(m: Eval[Measure[C]])(using Cost[C])
  case Set(ms: List[Measure[C]])(using Cost[C])

  def tainted = this match
    case Tainted(_) => this
    case Set(ms)    => Tainted(Eval.now(ms.head))

  def lifted(f: Measure[C] => Measure[C]): MeasureSet[C] = this match
    case Tainted(m) => Tainted(Eval.later(f(m.value)))
    case Set(ms)    => Set(ms.map(f))

  def concat(other: MeasureSet[C]) = (this, other) match
    case (_, Tainted(_))      => this
    case (Tainted(_), Set(_)) => other
    case (Set(ms0), Set(ms1)) => Set(concatMeasures(ms0, ms1))

  def layouted = this match
    case Tainted(m) => layout(m.value.doc, 0, 0)
    case Set(ms)    => layout(ms.head.doc, 0, 0)

def dedup[C: Cost](ms: List[Measure[C]]): List[Measure[C]] =
  ms match
    case m0 :: m1 :: tail =>
      if (m1 <= m0) {
        dedup(m1 :: tail)
      } else {
        m0 :: dedup(m1 :: tail)
      }
    case _ => ms

def concatMeasures[C: Cost](
    a: List[Measure[C]],
    b: List[Measure[C]]
): List[Measure[C]] = (a, b) match
  case (Nil, _) => b
  case (_, Nil) => a
  case (m0 :: tl0, m1 :: tl1) =>
    if (m0 <= m1) {
      concatMeasures(a, tl1)
    } else if (m1 <= m0) {
      concatMeasures(tl0, b)
    } else if (m0.last > m1.last) {
      m0 :: concatMeasures(tl0, b)
    } else {
      m1 :: concatMeasures(a, tl1)
    }

def resolve[C: Cost](ds: Doc, c: Int, i: Int): MeasureSet[C] =
  ds match
    case Doc.Text(s) =>
      if (c + s.length() <= Cost[C].cw && i <= Cost[C].cw) {
        MeasureSet.Set(List(ds.measured(c, i)))
      } else {
        MeasureSet.Tainted(Eval.later(ds.measured(c, i)))
      }
    case Doc.Nl() =>
      if (c <= Cost[C].cw && i <= Cost[C].cw) {
        MeasureSet.Set(List(ds.measured(c, i)))
      } else {
        MeasureSet.Tainted(Eval.later(ds.measured(c, i)))
      }
    case Doc.Nest(n, d) =>
      d.resolved(c, i + n).lifted(_.nested(n))
    case Doc.Align(d) =>
      if (i <= Cost[C].cw) {
        d.resolved(c, c).lifted(_.aligned)
      } else {
        d.resolved(c, c).tainted.lifted(_.aligned)
      }
    case Doc.Concat(a, b) => {
      a.resolved(c, i) match
        case MeasureSet.Tainted(ma) =>
          b.resolved(ma.value.last, i).tainted match
            case MeasureSet.Tainted(mb) =>
              MeasureSet.Tainted(Eval.later(ma.value concat mb.value))
            case _ => throw new IllegalStateException

        case MeasureSet.Set(ms) =>
          ms.map(resolveConcat(_, b, i))
            .reduceOption(_ concat _)
            .getOrElse(MeasureSet.Set(List.empty))
    }
    case Doc.Choice(a, b) =>
      a.resolved(c, i) concat b.resolved(c, i)

def resolveConcat[C: Cost](m: Measure[C], d: Doc, i: Int): MeasureSet[C] =
  d.resolved(m.last, i) match
    case MeasureSet.Tainted(mb) =>
      MeasureSet.Tainted(Eval.later(m concat mb.value))
    case MeasureSet.Set(ms) => MeasureSet.Set(dedup(ms.map(m concat _)))
