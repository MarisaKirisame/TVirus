package zombie.tvirus.prettier

import cats.Eval
import cats.kernel.Order
import cats.syntax.partialOrder.*

import scala.collection.mutable

enum Doc:
  case Text(s: String)
  case Newline(s: Option[String])
  case Nest(n: Int, d: Doc)
  case Align(d: Doc)
  case Concat(a: Doc, b: Doc)
  case Choice(a: Doc, b: Doc)
  case Fail

  def <>(other: Doc) = Concat(this, other)
  def <|>(other: Doc) = (this, other) match
    case (Fail, _) => other
    case (_, Fail) => this
    case _         => Choice(this, other)
  def <+>(other: Doc) = this <> Align(other)
  def <%>(other: Doc) = this <> Doc.HardNl <> other
  def <->(other: Doc) = Doc.Flatten(this) <+> other

  def resolved[C: Cost]: String =
    Resolver().resolve(this, 0, 0).layouted.mkString("\n")

private def memoize[I, O](f: I => O): I => O = new mutable.HashMap[I, O]() {
  override def apply(key: I) = getOrElseUpdate(key, f(key))
}

object Doc:
  val Nl = Newline(Some(" "))
  val Break = Newline(Some(""))
  val HardNl = Newline(None)
  val SBreak = " " <|> Doc.Nl

  def Group(d: Doc) = d <|> Flatten(d)

  lazy val Flatten: Doc => Doc = memoize { ds =>
    ds match
      case Fail | Text(_)   => ds
      case Newline(None)    => Fail
      case Newline(Some(s)) => Text(s)
      case Nest(n, d)       => Flatten(d)
      case Align(d)         => Flatten(d)
      case Concat(a, b)     => Flatten(a) <> Flatten(b)
      case Choice(a, b)     => Flatten(a) <|> Flatten(b)
  }

  def hcat(ds: Iterable[Doc]) = ds.reduceOption(_ <-> _).getOrElse(Text(""))
  def vcat(ds: Iterable[Doc]) = ds.reduceOption(_ <%> _).getOrElse(Text(""))

  def bracketed(d: Doc, o: String = "(", e: String = ")", space: Boolean = false) = {
    val spc = if (space) { " " } else { "" }
    (o <> spc <> d <> spc <> e) <|> (o <> Nest(2, Doc.Nl <> d) <> Doc.Nl <> e)
  }
    

given Conversion[String, Doc] = Doc.Text.apply

def layout(ds: Doc, c: Int, i: Int): List[String] = ds match
  case Doc.Text(s)    => List(s)
  case Doc.Newline(_) => List("", " " * i)
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
  case Doc.Fail         => throw new UnsupportedOperationException

trait Cost[T] extends Order[T]:
  val nl: T
  def text(c: Int, l: Int): T

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

    extension (a: (Int, Int))
      override def +(b: (Int, Int)): (Int, Int) = (a(0) + b(0), a(1) + b(1))
  }

case class Measure[C: Cost](last: Int, cost: C, doc: Doc):
  def concat(other: Measure[C]) =
    Measure(other.last, cost + other.cost, doc <> other.doc)

  def nested(n: Int) =
    Measure(last, cost, Doc.Nest(n, doc))

  def aligned =
    Measure(last, cost, Doc.Align(doc))

  def <=(other: Measure[C]): Boolean = last <= other.last && cost <= other.cost

def measure[C: Cost](ds: Doc, c: Int, i: Int): Measure[C] = ds match
  case Doc.Text(s) => Measure(c + s.length(), Cost[C].text(c, s.length()), ds)
  case Doc.Newline(_) => Measure(i, Cost[C].nl + Cost[C].text(0, i), ds)
  case Doc.Nest(n, d) => measure(d, c, i).nested(n)
  case Doc.Align(d)   => measure(d, c, c).aligned
  case Doc.Concat(a, b) => {
    val ma = measure(a, c, i)
    val mb = measure(b, ma.last, i)
    ma concat mb
  }
  case Doc.Choice(a, b) => throw new UnsupportedOperationException
  case Doc.Fail         => throw new UnsupportedOperationException

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

class Resolver[C: Cost](
    w: Int = 100
) {
  private val resolveCache: mutable.HashMap[(Doc, Int, Int), MeasureSet[C]] =
    mutable.HashMap.empty
  private val resolveConcatCache
      : mutable.HashMap[(Measure[C], Doc, Int), MeasureSet[C]] =
    mutable.HashMap.empty

  def resolve(ds: Doc, c: Int, i: Int) =
    resolveCache.getOrElseUpdate((ds, c, i), resolveImpl(ds, c, i))

  private def resolveImpl(ds: Doc, c: Int, i: Int): MeasureSet[C] = ds match
    case Doc.Text(s) =>
      if (c + s.length() <= w && i <= w) {
        MeasureSet.Set(List(measure(ds, c, i)))
      } else {
        MeasureSet.Tainted(Eval.later(measure(ds, c, i)))
      }
    case Doc.Newline(_) =>
      if (c <= w && i <= w) {
        MeasureSet.Set(List(measure(ds, c, i)))
      } else {
        MeasureSet.Tainted(Eval.later(measure(ds, c, i)))
      }
    case Doc.Nest(n, d) =>
      resolve(d, c, i + n).lifted(_.nested(n))
    case Doc.Align(d) =>
      if (i <= w) {
        resolve(d, c, c).lifted(_.aligned)
      } else {
        resolve(d, c, c).tainted.lifted(_.aligned)
      }
    case Doc.Concat(a, b) => {
      resolve(a, c, i) match
        case MeasureSet.Tainted(ma) =>
          resolve(b, ma.value.last, i).tainted match
            case MeasureSet.Tainted(mb) =>
              MeasureSet.Tainted(Eval.later(ma.value concat mb.value))
            case _ => throw new IllegalStateException

        case MeasureSet.Set(ms) =>
          ms.map(resolveConcat(_, b, i))
            .reduceOption(_ concat _)
            .getOrElse(MeasureSet.Set(List.empty))
    }
    case Doc.Choice(a, b) =>
      resolve(a, c, i) concat resolve(b, c, i)
    case Doc.Fail => throw new UnsupportedOperationException

  def resolveConcat(m: Measure[C], d: Doc, i: Int): MeasureSet[C] =
    resolveConcatCache.getOrElseUpdate((m, d, i), resolveConcatImpl(m, d, i))

  private def resolveConcatImpl(m: Measure[C], d: Doc, i: Int): MeasureSet[C] =
    resolve(d, m.last, i) match
      case MeasureSet.Tainted(mb) =>
        MeasureSet.Tainted(Eval.later(m concat mb.value))
      case MeasureSet.Set(ms) => MeasureSet.Set(dedup(ms.map(m concat _)))
}
