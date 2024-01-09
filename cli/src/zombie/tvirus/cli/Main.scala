package zombie.tvirus.cli

import java.io.{File, FileReader, FileWriter, IOException}
import org.antlr.v4.runtime.CharStreams
import mainargs.{main, arg, ParserForMethods, Flag}

import zombie.tvirus.parser.drive
import zombie.tvirus.codegen.*

object Main {
  @main
  def run(
      @arg(positional = true, doc = "source-file")
      source_file: String,
      @arg(short = 'o', doc = "target-file")
      target_file: String = "a.cpp"
  ) = {
    val fileReader = new FileReader(new File(source_file))

    var prog = refresh(cons(drive(CharStreams.fromReader(fileReader))))
    prog = let_simplification(merge_abs_app(cps(unnest_match(prog))))

    try {
      val fileWriter = new FileWriter(target_file)

      fileWriter.write(codegen(prog))

      fileWriter.close()
    } catch {
      case e: IOException =>
        println("fail to write into target-file" + e.getMessage)
    }

  }
  def main(args: Array[String]): Unit = ParserForMethods(this).runOrExit(args)
}
