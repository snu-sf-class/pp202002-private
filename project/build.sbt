lazy val root = (project in file(".")).settings(
  inThisBuild(
    List(
      organization := "pp202002.project",
      scalaVersion := "2.13.4"
    )
  ),
  name := "pp202002-project"
)

libraryDependencies += "org.parboiled" %% "parboiled" % "2.2.1"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.2" % Test
