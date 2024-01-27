package zombie.tvirus.prettier

import utest.*

object PrettierTests extends TestSuite {

  def printDocChoice(w: Int) = {
    val exit_d = Doc.Text("exit();")

    val d = "while (true) {" <> Doc.Nest(
      4,
      Doc.Nl <> "f();" <> Doc.Nl <> "if (done())" <>
        ((" " <> exit_d) <|> Doc.Nest(4, Doc.Nl <> exit_d))
    ) <> Doc.Nl <> "}"

    d.resolved(using Cost.sumOfSquared(w))
  }

  def printDocGroup(w: Int) = {
    val d = "while (true) {" <> Doc.Nest(
      4,
      Doc.Nl <> "f();" <> Doc.Nl <> "if (done())" <>
        Doc.Group(Doc.Nest(4, Doc.Nl <> "exit();"))
    ) <> Doc.Nl <> "}"

    d.resolved(using Cost.sumOfSquared(w))
  }

  enum SExp:
    case Atom(s: String)
    case List(ls: scala.List[SExp])

  def printSExp(s: SExp, w: Int) = {
    def acat(ds: List[Doc]) =
      ds.reduceOption(_ <+> " " <+> _).getOrElse(Doc.Text(""))

    def pretty(s: SExp): Doc = s match
      case SExp.Atom(s) => Doc.Text(s)
      case SExp.List(ls) =>
        ls match
          case Nil      => "(" <+> ")"
          case x :: Nil => "(" <+> pretty(x) <+> ")"
          case x :: xs => {
            val xd = pretty(x)
            val xsd = xs.map(pretty(_))
            "(" <+>
              (acat(xd :: xsd) <|>
                Doc.vcat(xd :: xsd) <|>
                (xd <+> " " <+> Doc.vcat(xsd)))
              <+> ")"
          }

    pretty(s).resolved(using Cost.sumOfSquared(w))
  }

  val exampleSExp = SExp.List(
    List(SExp.Atom("a"), SExp.Atom("b"), SExp.Atom("c"), SExp.Atom("d"))
  )

  val tests = Tests {
    test("choiceDoc80") {
      val pd = printDocChoice(80)
      assert(
        pd ==
          """while (true) {
            |    f();
            |    if (done()) exit();
            |}""".stripMargin
      )
      pd
    }
    test("choiceDoc20") {
      val pd = printDocChoice(20)
      assert(
        pd ==
          """while (true) {
            |    f();
            |    if (done())
            |        exit();
            |}""".stripMargin
      )
      pd
    }
    test("groupDoc80") {
      val pd = printDocGroup(80)
      assert(
        pd ==
          """while (true) {
            |    f();
            |    if (done()) exit();
            |}""".stripMargin
      )
      pd
    }
    test("groupDoc20") {
      val pd = printDocGroup(20)
      assert(
        pd ==
          """while (true) {
            |    f();
            |    if (done())
            |        exit();
            |}""".stripMargin
      )
      pd
    }
    test("sExp4") {
      val pd = printSExp(exampleSExp, 4)
      assert(
        pd ==
          """(a
            | b
            | c
            | d)""".stripMargin
      )
      pd
    }
    test("sExp6") {
      val pd = printSExp(exampleSExp, 6)
      assert(
        pd ==
          """(a b
            |   c
            |   d)""".stripMargin
      )
      pd
    }
    test("sExp10") {
      val pd = printSExp(exampleSExp, 10)
      assert(
        pd ==
          """(a b c d)""".stripMargin
      )
      pd
    }
  }
}
