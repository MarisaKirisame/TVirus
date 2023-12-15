import mill._, scalalib._
import $file.common, common._

object parser extends AntlrModule with CommonModule {
  override def antlrGrammarSources = T.sources {
    Seq(millSourcePath / "antlr4").map(PathRef(_))
  }
  override def antlrPackage: Option[String] = Some("zombie.tvirus.parser.generated")
}

object codegen extends CommonModule {
  def moduleDeps = Seq(parser)
}
