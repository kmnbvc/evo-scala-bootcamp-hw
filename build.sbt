
lazy val root = (project in file("."))
  .enablePlugins(sbt.plugins.BulkySourcesPlugin)
  .settings(
    name := "evo-scala-bootcamp-homeworks",
    scalaVersion := "2.13.3",
    version := "1.0",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.2" % "test"
  )
