import org.ensime.EnsimeKeys._
import scalariform.formatter.preferences._

import org.ensime.EnsimeCoursierKeys._
ensimeServerVersion in ThisBuild := "2.0.0-SNAPSHOT" // or "1.0.1"
ensimeProjectServerVersion in ThisBuild := "2.0.0-SNAPSHOT" // or "1.0.1"
ensimeJavaFlags in ThisBuild += "-Xmx4G"

cancelable in Global := true

// http://www.scalatest.org/user_guide/using_the_runner
// (-o) - standard output
//   D - show all durations
//   F - show full stack traces
testOptions in Test ++= Seq(Tests.Argument(TestFrameworks.ScalaTest,"-oDF"))
