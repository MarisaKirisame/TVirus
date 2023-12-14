import mill._, scalalib._

trait CommonModule extends ScalaModule {
    def scalaVersion = "3.3.1"
}

object parser extends CommonModule {
    def scalaVersion = "3.3.1"

    def moduleDeps = Seq(prettier)
}

object prettier extends CommonModule {
    def scalaVersion = "3.3.1"
}