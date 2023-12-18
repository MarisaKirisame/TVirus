import mill._, scalalib._
import $file.common, common._

object parser extends AntlrModule with CommonModule {
  override def antlrGrammarSources = T.sources {
    Seq(millSourcePath / "antlr4").map(PathRef(_))
  }
  override def antlrPackage = Some("zombie.tvirus.parser.generated")
  override def antlrGenerateVisitor = true
}

object codegen extends CommonModule {
  def ivyDeps = Agg(
    ivy"dev.zio::zio-prelude:1.0.0-RC21"
  )

  def moduleDeps = Seq(parser)
}

object prettier extends CommonModule {
}

object cli extends CommonModule {
  def ivyDeps = Agg(
    ivy"com.lihaoyi::mainargs:0.5.4"
  )

  def moduleDeps = Seq(parser, codegen)
}
