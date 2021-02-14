package sbt_plugin

import sbt.Keys._
import sbt._

import scala.io.Source

object BulkySourcesPlugin extends AutoPlugin {

  val bulkyThresholdInLines = settingKey[Int]("Plugin shows source files with number of lines more than this value")
  val bulkySources = taskKey[Seq[(Int, File)]]("Run bulky sources task")

  lazy val bulkySourcesTask = Def.task {
    val threshold = bulkyThresholdInLines.value
    val srcFiles = sources.value
    val bulkyFiles = srcFiles
      .map(f => (lines(f), f))
      .filter(_._1 > threshold)
      .sortBy(_._1)(Ordering[Int].reverse)
    bulkyFiles
  }

  override val projectSettings = inConfig(Compile)(runTask) ++ inConfig(Test)(runTask)

  override val globalSettings = bulkyThresholdInLines := 100

  private def lines(f: File): Int = {
    val s = Source.fromFile(f)
    try {
      s.getLines().length
    } finally {
      s.close()
    }
  }

  private def runTask = bulkySources := bulkySourcesTask.value

}
