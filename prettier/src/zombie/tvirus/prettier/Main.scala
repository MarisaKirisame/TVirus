package zombie.tvirus.prettier

import scala.collection.immutable.List
import Description.*

def treeExample(): Unit = {
  case class Tree(name: String, sons: List[Tree])

  def describeTree(tree: Tree): Description = {
    if tree.sons.isEmpty then
      Text(tree.name)
    else {
      val sons = tree.sons.map(describeTree)
      val sonsDescription = sons.reduce((a, b) => a <> Text(",") </> b)
      val showBracket = (x: Description) => Text("[") <> Nest(1, x) <> Text("]")
      group(Text(tree.name) <> Nest(tree.name.length, showBracket(sonsDescription)))
    }
  }

  val tree = Tree("aaa", List(
    Tree("bbb", List(
      Tree("ccc", List()),
      Tree("ddd", List(
        Tree("eee", List()))))),
    Tree("fff", List(
      Tree("ggg", List()),
      Tree("hhh", List()),
      Tree("iii", List()))),
    Tree("jjj", List()),
    Tree("kkk", List()),
    Tree("lll", List()),
  ))

  println(layout(best(10, describeTree(tree))))
  println(layout(best(30, describeTree(tree))))
  println(layout(best(100, describeTree(tree))))
}

def xmlExample(): Unit = {
  case class Attr(label: String, value: String)

  sealed abstract class XML
  case class Element(name: String, attrs: List[Attr], childs: List[XML]) extends XML
  case class TextXML(text: String) extends XML

  def showXML(xml: XML): Description = {
    def showAttr(attr: Attr) =
      Text(attr.label) <> Text("=") <> Text("\"") <> Text(attr.value) <> Text("\"")

    extension (ds: List[Description])
      def foldShow(): Description = {
        if ds.isEmpty then
          Nil()
        else {
          ds.drop(1).foldLeft(ds.head)((a, b) => a </> b)
        }
      }

    xml match
      case TextXML(text) => text.split(' ').toList.map(Text.apply).foldShow()
      case Element(name, attrs, childs) =>
        val lable_prefix = if attrs.isEmpty then "<" + name else "<" + name + " "
        if childs.isEmpty then
          bracketStr(lable_prefix, group(attrs.map(showAttr).foldShow()), "/>")
        else
          bracketStr(lable_prefix, group(attrs.map(showAttr).foldShow()), ">") <>
            Nest(2, Line() <> group(childs.map(showXML).foldShow())) </>
            Text("</") <> Text(name) <> Text(">")
  }

  val xml = Element("html", List(), List(
    Element("head", List(), List(
      Element("title", List(), List(
        TextXML("helloworld")
      )),
      Element("body", List(), List(
        Element("h1", List(), List(
          TextXML("Hello World")
        )),
        Element("a", List(Attr("href", "http://www.google.com")), List(
          TextXML("Google")
        )),
        Element("img", List(
          Attr("src", "link"),
          Attr("alt", "cat"),
          Attr("width", "500"),
          Attr("height", "600")), List()
        )
      ))
    ))
  ))

  println(layout(best(100, showXML(xml))))
  println(layout(best(300, showXML(xml))))
}

@main
def main(): Unit = {
  treeExample()

  xmlExample()
}