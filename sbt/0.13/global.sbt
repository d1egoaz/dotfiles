cancelable in Global := true

testOptions in Test ++= Seq(Tests.Argument(TestFrameworks.ScalaTest,"-oDF"))
