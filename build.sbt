name := "teatimer-terminal"

version := "1.0"

scalaVersion := "3.1.0"

mainClass in (Compile, packageBin) := Some("me.mycf.teatimer.terminal.Main")

assemblyMergeStrategy in assembly := {
  case PathList("META-INF", xs @ _*) => MergeStrategy.discard
  case x => MergeStrategy.first
}