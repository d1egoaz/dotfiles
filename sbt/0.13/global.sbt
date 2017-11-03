import org.ensime.EnsimeKeys._

// http://www.scalatest.org/user_guide/using_the_runner
// (-o) - standard output
//   D - show all durations
//   F - show full stack traces
// testOptions in Test ++= Seq(Tests.Argument(TestFrameworks.ScalaTest,"-oDF"))
testOptions in Test ++= Seq(Tests.Argument(TestFrameworks.ScalaTest,"-oD"))

ensimeJavaFlags in ThisBuild := Seq("-Xss2m", "-Xmx3g", "-XX:MaxMetaspaceSize=1024m")
ensimeIgnoreScalaMismatch in ThisBuild := true

cancelable in Global := true

// import scalafix.sbt.ScalafixPlugin.autoImport._
// sbtfixSettings // enable semanticdb-sbt for sbt metabuilds.

