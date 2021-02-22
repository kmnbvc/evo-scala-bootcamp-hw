package sbt.plugins

import sbt.Keys._
import sbt.{Def, _}

object BulkySourcesPlugin extends AutoPlugin {

  object autoImport {
    val bulkyThresholdInLines = settingKey[Int]("Plugin shows source files with number of lines more than this value")
    val bulkySources = taskKey[Seq[(Int, File)]]("Run bulky sources task")
  }

  import autoImport._

  override def trigger = allRequirements

  lazy val bulkySourcesTask = Def.task {
    val threshold = bulkyThresholdInLines.value
    sources.value.map(countLinesTuple).filter(exceed(threshold)).sorted(ordering)
  }

  override def projectSettings: Seq[Def.Setting[_]] =
    Seq(bulkyThresholdInLines := 100) ++
    inConfig(Compile)(taskSetting) ++
    inConfig(Test)(taskSetting)

  private val taskSetting = bulkySources := bulkySourcesTask.value
  private val countLines: File => Int = f => IO.readLines(f).length
  private val countLinesTuple: File => (Int, File) = f => (countLines(f), f)
  private val exceed: Int => ((Int, File)) => Boolean = threshold => {
    case (linesNumber, _) => linesNumber > threshold
  }
  private val ordering = Ordering[(Int, File)].reverse
}
