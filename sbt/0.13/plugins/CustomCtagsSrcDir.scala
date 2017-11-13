import sbt._
import Keys._
import net.ceedubs.sbtctags.CtagsKeys._

object CustomCtagsSrcDir extends Plugin {
  override def settings = Seq(
    ctagsGeneration := { _ => () },
    dependencySrcUnzipDir := file("gen-ctags")
  )
}
