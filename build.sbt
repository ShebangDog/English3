ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.1"

lazy val root = (project in file("."))
  .settings(
    name := "English",
    idePackagePrefix := Some("dog.shebang.english")
  )

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest-diagrams" % "3.2.16",
  "org.scalatest" %% "scalatest-funspec" % "3.2.15",
)
