package zombie.tvirus.prettier

case class Tree(name: String, sons: List[Tree])

def describeTree(tree: Tree): Description = {
  if tree.sons.isEmpty then
    Text(tree.name)
  else {
    val sons = tree.sons.map(describeTree)
    val sonsDescription = sons.reduce((a, b) => Concat(a, Concat(Concat(Text(","), Gap()), b)))
    val showBracket = (x: Description) => Concat(Concat(Text("["), Nest(1, x)), Text("]"))
    group(Concat(Text(tree.name), Nest(tree.name.length, showBracket(sonsDescription))))
  }
}

@main
def main(): Unit = {
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