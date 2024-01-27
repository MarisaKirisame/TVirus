//> using scala 3.3
//> using dep "com.lihaoyi::os-lib:0.9.3"

val allTVirusFiles = os.walk(os.pwd / "example").filter(_.ext == "tv")

// os.proc.call will throw SubprocessException if non-zero exit code is returned
if (
  allTVirusFiles
    .map(path => {
      println(
        s"============================== Compiling $path =============================="
      )
      try {
        os.proc("mill", "cli", path).call()
        true
      } catch {
        case e: os.SubprocessException => {
          println(s"failed for $path")
          false
        }
      }
    })
    .reduce((a, b) => a && b)
) {
  println("all compilation attempts succeeded")
} else {
  println("failed")
  throw Exception()
}
