import sbt._
import org.ensime.Imports.EnsimeKeys
import scalariform.formatter.preferences._

object EnsimeProjectSettings extends AutoPlugin {
  override def requires = org.ensime.EnsimePlugin
  override def trigger = allRequirements
  override def projectSettings = Seq(
    EnsimeKeys.scalariformPreferences := EnsimeKeys.scalariformPreferences.value
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
  )
}
