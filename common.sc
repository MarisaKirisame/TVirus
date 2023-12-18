import mill._, scalalib._
import scala.collection.mutable
import $ivy.`org.antlr:antlr4:4.13.1`
import org.antlr.v4.tool.{ANTLRMessage, ANTLRToolListener}

trait CommonModule extends ScalaModule {
  def scalaVersion = "3.3.1"
}

trait AntlrModule extends JavaModule {
  def antlrGrammarSources: Target[Seq[PathRef]]
  def antlrPackage: Option[String] = None
  def antlrGenerateVisitor: Boolean = false
  def antlrGenerateListener: Boolean = false

  def antlrGrammarSourceFiles = T {
    antlrGrammarSources()
      .flatMap { source =>
        if (os.isDir(source.path)) {
          os.walk(source.path)
        } else {
          Seq(source.path)
        }
      }
      .filter { path =>
        os.isFile(path) && path.ext == "g4"
      }
      .map(PathRef(_))
  }

  def antlrGenerate = T {
    val antlrToolArgs = mutable.ArrayBuffer.empty[String]

    antlrToolArgs.appendAll(
      antlrGrammarSourceFiles().map(_.path.relativeTo(os.pwd).toString)
    )

    antlrToolArgs.append("-o")
    antlrToolArgs.append(s"${T.dest}")
    antlrToolArgs.append("-Xexact-output-dir")

    antlrToolArgs.append("-lib")
    antlrToolArgs.append(s"${T.dest}")

    if (!antlrGenerateVisitor) {
      antlrToolArgs.append("-no-visitor")
    }

    if (!antlrGenerateListener) {
      antlrToolArgs.append("-no-listener")
    }

    if (antlrPackage.isDefined) {
      antlrToolArgs.append("-package")
      antlrToolArgs.append(antlrPackage.get)
    }

    val antlrTool = new org.antlr.v4.Tool(antlrToolArgs.toArray)
    antlrTool.addListener(new ToolListener())
    antlrTool.processGrammarsOnCommandLine()

    os.walk(T.dest)
      .filter(path => os.isFile(path) && path.ext == "java")
      .map(PathRef(_))
  }

  override def generatedSources = T {
    super.generatedSources() ++ antlrGenerate()
  }

  override def ivyDeps = super.ivyDeps() ++ Seq(
    ivy"org.antlr:antlr4-runtime:4.13.1"
  )

  class ToolListener extends ANTLRToolListener {
    override def info(msg: String): Unit = throw new RuntimeException(msg)
    override def error(msg: ANTLRMessage): Unit = throw new RuntimeException(
      msg.toString
    )
    override def warning(msg: ANTLRMessage): Unit = throw new RuntimeException(
      msg.toString
    )
  }
}
