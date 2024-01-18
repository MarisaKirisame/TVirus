import mill._, scalalib._
import $file.common, common._

object parser extends AntlrModule with CommonModule {
  override def antlrGrammarSources = T.sources {
    Seq(millSourcePath / "antlr4").map(PathRef(_))
  }
  override def antlrPackage = Some("zombie.tvirus.parser.generated")
  override def antlrGenerateVisitor = true
  override def ivyDeps = super.ivyDeps() ++ Agg(ivy"com.lihaoyi::pprint:0.8.1")
}

object codegen extends CommonModule {
  def ivyDeps = Agg(
    ivy"org.typelevel::cats-core:2.10.0",
    ivy"org.typelevel::cats-collections-core:0.9.8"
  )

  def moduleDeps = Seq(parser)
}

object prettier extends CommonModule {
}

object cli extends CommonModule {
  def ivyDeps = Agg(
    ivy"com.lihaoyi::mainargs:0.5.4",
    ivy"com.lihaoyi::os-lib:0.9.3"
  )

  def moduleDeps = Seq(parser, codegen)
}
