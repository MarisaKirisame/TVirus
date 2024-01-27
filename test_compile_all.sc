//> using scala 3.3
//> using dep "com.lihaoyi::os-lib:0.9.3"

val allTVirusFiles = os.walk(os.pwd / "example").filter(_.ext == "tv")

// os.proc.call will throw SubprocessException if non-zero exit code is returned

val results = allTVirusFiles
    .map(p => {
      try {
        os.proc("mill", "cli", p).call()
        p -> true
      } catch {
        case e: os.SubprocessException => p -> false
      }
    })
val textresult = results.map((p, succ) => {
      val ps = p.toString().split("/")
      val pc = ps(ps.length - 1)
      val pn = pc.slice(0, pc.length - 3)
      val s = if (succ) { "succeeded" }
      else { "failed" }
      s"$pn: $s\n"
    })
    .reduce((a, b) => a ++ b)
print(textresult)

if (!results.map((p, suc) => suc).reduce((a, b) => a && b)) {
  throw Exception()
}
