lazy val root = project
  .in(file("."))
  .settings(
    name := "advent-of-code-2022",
    version := "0.0.1-SNAPSHOT",
    scalaVersion := "3.2.1",
    scalacOptions ++= Seq("--deprecation"),
    autoScalaLibrary := true,
    libraryDependencies ++= Seq(
      "org.scala-lang.modules" %% "scala-parser-combinators" % "2.1.1",
      "org.scalatest" %% "scalatest-funsuite" % "3.2.14" % Test
    ),
    run / fork := true,
    run / connectInput := true
  )
