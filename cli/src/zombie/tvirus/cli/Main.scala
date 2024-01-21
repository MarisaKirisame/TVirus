package zombie.tvirus.cli

import org.antlr.v4.runtime.CharStreams
import mainargs.{main, arg, ParserForMethods}

import zombie.tvirus.parser.drive
import zombie.tvirus.codegen.*

extension [A, B](a: A) infix def |>(f: A => B): B = f(a)

object Main {
  @main
  def compile(
      @arg(positional = true, doc = "source-file")
      source_file: String,
      @arg(short = 'o', doc = "target-file")
      target_file: String = "a.cpp"
  ) = {

    val p = os.read(os.Path(source_file, os.pwd))
      |> CharStreams.fromString
      |> drive
      |> cons
      |> refresh
      |> unnest_match
      |> cps
      |> reify_global_funcs
      |> merge_abs_app
      |> let_simplification
      |> codegen

    os.write.over(os.Path(target_file, os.pwd), p)
  }

  def main(args: Array[String]): Unit = ParserForMethods(this).runOrExit(args)
}
