
lazy val root = (project in file("."))
  .settings(
    name := "evo-scala-bootcamp-homeworks",
    scalaVersion := "2.13.3",
    version := "1.0",
    libraryDependencies += "org.typelevel" %% "cats-core" % "2.4.2",
    libraryDependencies += "com.chuusai" %% "shapeless" % "2.3.3",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.2" % "test",
    addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.11.3" cross CrossVersion.full)
  )
