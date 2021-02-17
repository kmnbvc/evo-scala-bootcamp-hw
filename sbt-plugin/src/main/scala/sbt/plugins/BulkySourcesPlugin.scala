package sbt.plugins

import sbt.Keys._
import sbt.{Def, _}

object BulkySourcesPlugin extends AutoPlugin {

  val bulkyThresholdInLines = settingKey[Int]("Plugin shows source files with number of lines more than this value")
  val bulkySources = taskKey[Seq[(Int, File)]]("Run bulky sources task")

  lazy val bulkySourcesTask = Def.task {
    val threshold = bulkyThresholdInLines.value
    sources.value.map(countLinesTuple).filter(exceed(threshold)).sorted(ordering)
  }

  override def projectSettings: Seq[Def.Setting[_]] = inConfig(Compile)(taskSetting) ++ inConfig(Test)(taskSetting)
  override def globalSettings: Seq[Def.Setting[_]] = bulkyThresholdInLines := 100

  private val taskSetting = bulkySources := bulkySourcesTask.value
  private val countLines: File => Int = f => IO.readLines(f).length
  private val countLinesTuple: File => (Int, File) = f => (countLines(f), f)
  private val exceed: Int => ((Int, File)) => Boolean = threshold => {
    case (linesNumber, _) => linesNumber > threshold
  }
  private val ordering = Ordering[(Int, File)].reverse
}
