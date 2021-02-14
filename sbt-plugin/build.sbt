lazy val root = (project in file("."))
  .enablePlugins(SbtPlugin)
  .settings(
    name := "sbt-bulky-sources-plugin",
    scalaVersion := "2.12.3",
    version := "1.0"
  )
