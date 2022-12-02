lazy val root = project
  .in(file("."))
  .settings(
    name := "advent-of-code-2022",
    version := "0.0.1-SNAPSHOT",
    scalaVersion := "3.2.1",
    scalacOptions ++= Seq("--deprecation"),
    autoScalaLibrary := true,
    run / fork := true,
    run / connectInput := true
  )
