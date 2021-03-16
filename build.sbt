
val circeDeps = Seq(
  "io.circe" %% "circe-core",
  "io.circe" %% "circe-generic",
  "io.circe" %% "circe-generic-extras",
  "io.circe" %% "circe-parser"
).map(_ % "0.13.0")

val catsDeps = Seq(
  "org.typelevel" %% "cats-core" % "2.4.2"
)

val testDeps = Seq(
  "org.scalatest" %% "scalatest" % "3.2.2" % Test
)

lazy val root = (project in file("."))
  .settings(
    name := "evo-scala-bootcamp-homeworks",
    scalaVersion := "2.13.3",
    version := "1.0",
    scalacOptions += "-Ymacro-annotations",
    libraryDependencies ++= catsDeps,
    libraryDependencies ++= circeDeps,
    libraryDependencies ++= Seq(
      "com.chuusai" %% "shapeless" % "2.3.3",
      "org.scalaj" %% "scalaj-http" % "2.4.2"
    ),
    libraryDependencies ++= testDeps,
    addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.11.3" cross CrossVersion.full),
    run / fork := true
  )
