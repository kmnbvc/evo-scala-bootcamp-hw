
lazy val root = (project in file("."))
  .settings(
    name := "evo-scala-bootcamp-homeworks",
    scalaVersion := "2.13.3",
    version := "1.0",
    libraryDependencies += "com.chuusai" %% "shapeless" % "2.3.3",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.2" % "test"
  )
