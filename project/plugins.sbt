lazy val root = (project in file(".")).dependsOn(bulkySourcesPlugin)
lazy val bulkySourcesPlugin = RootProject(file("../sbt-plugin"))
addSbtPlugin("sbt-bulky-sources-plugin" % "sbt-bulky-sources-plugin" % "1.0")
