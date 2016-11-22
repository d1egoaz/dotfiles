import org.ensime.EnsimeKeys._
import scalariform.formatter.preferences._

cancelable in Global := true

// http://www.scalatest.org/user_guide/using_the_runner
// (-o) - standard output
//   D - show all durations
//   F - show full stack traces
testOptions in Test ++= Seq(Tests.Argument(TestFrameworks.ScalaTest,"-oDF"))


// Ensime
ensimeScalaVersion := "2.11.8"
ensimeJavaFlags += "-Xmx2048m"

// scalariformPreferences
scalariformPreferences := scalariformPreferences.value
  .setPreference(AlignParameters, false)
  // .setPreference(AlignArguments, false)
  .setPreference(AlignSingleLineCaseStatements, false)
  .setPreference(CompactControlReadability, false)
  .setPreference(CompactStringConcatenation, false)
  // .setPreference(DanglingCloseParenthesis, "Force")
  .setPreference(DoubleIndentClassDeclaration, false)
  // .setPreference(DoubleIndentMethodDeclaration, false)
  .setPreference(FormatXml, true)
  .setPreference(IndentLocalDefs, false)
  .setPreference(IndentPackageBlocks, true)
  .setPreference(IndentSpaces, 2)
  .setPreference(IndentWithTabs, false)
  .setPreference(MultilineScaladocCommentsStartOnFirstLine, true)
  // .setPreference(NewlineAtEndOfFile, true)
  .setPreference(PlaceScaladocAsterisksBeneathSecondAsterisk, true)
  .setPreference(PreserveSpaceBeforeArguments, false)
  .setPreference(RewriteArrowSymbols, false)
  .setPreference(SpaceBeforeColon, false)
  .setPreference(SpaceInsideBrackets, false)
  .setPreference(SpaceInsideParentheses, false)
  .setPreference(SpacesWithinPatternBinders, false)
  // .setPreference(SpacesAroundMultiImports, false)
