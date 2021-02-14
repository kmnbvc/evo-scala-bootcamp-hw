
lazy val root = (project in file("."))
  .enablePlugins(SbtPlugin, sbt_plugin.BulkySourcesPlugin)
  .settings(
    name := "evo-scala-bootcamp-homeworks",
    scalaVersion := "2.12.3",
    version := "1.0",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.2" % "test",
    sbt_plugin.BulkySourcesPlugin.bulkyThresholdInLines := 20
  )
