import org.ensime.EnsimeKeys._
import scalariform.formatter.preferences._

cancelable in Global := true

// http://www.scalatest.org/user_guide/using_the_runner
// (-o) - standard output
//   D - show all durations
//   F - show full stack traces
testOptions in Test ++= Seq(Tests.Argument(TestFrameworks.ScalaTest,"-oDF"))
