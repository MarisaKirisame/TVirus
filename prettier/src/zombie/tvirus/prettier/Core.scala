package zombie.tvirus.prettier

import scala.annotation.tailrec
import scala.collection.immutable

def flatten(d: Description): Description = d match {
  case Nil() => Nil()
  case Line() => Text(" ")
  case Text(text) => Text(text)
  case Concat(left, right) => (flatten(left), flatten(right)) match {
    case (Nil(), right) => right
    case (left, Nil()) => left
    case (left, right) => Concat(left, right)
  }
  case Union(left, _) => flatten(left)
  case Nest(_, description) => flatten(description)
}

def group(d: Description) = Union(flatten(d), d)

def best(width: Int, d: Description): Document = {
  def helper(current: Int, ds: List[(Int, Description)]): Document = ds match
    case ::(head, tail) =>
      val (indent, description) = head
      description match
        case Nil() => helper(current, tail)
        case Line() => DLine(indent, ()=>helper(current, tail))
        case Text(text) => DText(text, ()=>helper(current + text.length, tail))
        case Concat(left, right) => helper(current, (indent, left)::(indent, right)::tail)
        case Nest(i, description) => helper(current, (indent + i, description)::tail)
        case Union(left, right) =>
          chooseBetter(width - current, x=>x,
            helper(current, (indent, left)::tail),
            ()=>helper(current, (indent, right)::tail)
          )

    case immutable.Nil => DNil()

  @tailrec
  def chooseBetter(space: Int, outside: Document=>Document, left: Document, right: ()=>Document): Document = {
    if space < 0 then
      right()
    else {
      left match
        case _: DNil => outside(left)
        case _: DLine => outside(left)
        case DText(text, _) =>
          chooseBetter(space - text.length, cont=>outside(DText(text, ()=>cont)), left.cont, right)
    }
  }

  helper(0, (0, d)::scala.Nil)
}

def layout(d: Document): String = d match
  case DNil() => ""
  case DText(text, _cont) => text ++ layout(d.cont)
  case DLine(indent, _cont) => "\n" ++ " " * indent + layout(d.cont)