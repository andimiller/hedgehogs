import xerial.sbt.Sonatype._
import sbtwelcome._

val runtimes       = List(JVMPlatform, JSPlatform)
val scalaVersions  = List("2.13.16", "3.6.3")
val y              = scala.Console.YELLOW
val c              = scala.Console.CYAN
val commonSettings = List(
  logo                       := s"""
            |     $y  ___  __   __   ___ $c      __   __   __  
            | |__|$y |__  |  \\ / _` |__ $c |__| /  \\ / _` /__` 
            | |  |$y |___ |__/ \\__> |___$c |  | \\__/ \\__> .__/ 
	    |				     
            |          they've got a lot of edges
            |
            |version: ${version.value}
            |target scala versions: ${scalaVersions.mkString(", ")}
            |target runtimes: ${runtimes.mkString(", ")}
            |""".stripMargin,
  logoColor                  := scala.Console.CYAN,
  usefulTasks                := Seq(
    UsefulTask("ta", "+ test", "cross-test all versions"),
    UsefulTask("fa", "scalafmtAll", "reformat all scala files"),
    UsefulTask("fs", "scalafmtSbt", "reformat all sbt files"),
    UsefulTask("pa", "+ publishSigned", "release all versions")
  ),
  crossScalaVersions         := scalaVersions,
  organization               := "net.andimiller",
  crossPaths                 := true,
  testFrameworks += new TestFramework("munit.Framework"),
  version                    := "0.4.1",
  scalaVersion               := "3.6.3",
  ThisBuild / scalafmtConfig := file(".scalafmt.conf"),
  useGpg                     := true,
  publishTo                  := sonatypePublishTo.value,
  licenses                   := Seq("MIT" -> url("https://opensource.org/licenses/MIT")),
  sonatypeProjectHosting     := Some(
    GitHubHosting("andimiller", "hedgehogs", "andi at andimiller dot net")
  ),
  developers                 := List(
    Developer(
      id = "andimiller",
      name = "Andi Miller",
      email = "andi@andimiller.net",
      url = url("http://andimiller.net")
    )
  ),
  libraryDependencies ++= List(
    "net.andimiller" %%% "munit-cats-effect-3-styles" % "1.0.2"  % Test,
    "org.scalameta"  %%% "munit"                      % "0.7.29" % Test,
    "co.fs2"         %%% "fs2-io"                     % "3.2.7"  % Test
  )
)

lazy val root = (project in file("."))
  .aggregate(core.js, core.jvm, mermaid.js, mermaid.jvm)
  .settings(commonSettings)
  .settings(
    crossScalaVersions := Nil,
    publish / skip     := true
  )

lazy val core = crossProject(runtimes: _*)
  .in(file("modules/core"))
  .settings(commonSettings: _*)
  .settings(
    name := "hedgehogs-core",
    libraryDependencies ++= List(
      "org.typelevel" %%% "cats-core" % "2.7.0"
    )
  )

lazy val mermaid = crossProject(runtimes: _*)
  .in(file("modules/mermaid"))
  .dependsOn(core)
  .settings(commonSettings: _*)
  .settings(
    name := "hedgehogs-mermaid"
  )
