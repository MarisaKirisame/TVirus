//> using scala 3.3
//> using dep "com.lihaoyi::os-lib:0.9.3"

val allTVirusFiles = os.walk(os.pwd / "example").filter(_.ext == "tv")

// os.proc.call will throw SubprocessException if non-zero exit code is returned
allTVirusFiles.foreach(os.proc("mill", "cli", _).call())
