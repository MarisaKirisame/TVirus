package zombie.tvirus.cli

import java.nio.file.{FileSystems, Files}
import collection.JavaConverters.asScalaIteratorConverter

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
      target_file: String = "a.cpp",
      @arg(short = 'l', doc = "library")
      library: String = "library"
  ) = {

    val libdir = FileSystems.getDefault.getPath(library)
    val libcontents = Files.list(libdir).iterator().asScala.filter(Files.isRegularFile(_)).map((f) => os.read(os.Path(f, os.pwd))).reduce((a, b) => a + b)

    val src = libcontents + os.read(os.Path(source_file, os.pwd))

    println("===source===")
    println(src)
    println("===end source===")

    val p = src
      |> CharStreams.fromString
      |> drive
      |> cons
      |> refresh
      |> simpl
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
