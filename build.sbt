import xerial.sbt.Sonatype._
import sbtwelcome._

val runtimes       = List(JVMPlatform, JSPlatform, NativePlatform)
val scalaVersions  = List("2.13.6", "3.4.2")
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
  version                    := "0.3.0",
  scalaVersion               := "3.4.2",
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
    "net.andimiller" %%% "munit-cats-effect-styles" % "2.0.0-M1"  % Test,
    "org.scalameta"  %%% "munit"                    % "1.0.0-M7" % Test,
    "co.fs2"         %%% "fs2-io"                   % "3.7.0"  % Test
  )
)

lazy val root = (project in file("."))
  .aggregate(core.js, core.jvm, core.native, circe.js, circe.jvm, circe.native)
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
      "org.typelevel" %%% "cats-core" % "2.9.0"
    )
  )

lazy val circe = crossProject(runtimes: _*)
  .in(file("modules/circe"))
  .dependsOn(core)
  .settings(commonSettings: _*)
  .settings(
    name := "hedgehogs-circe",
    libraryDependencies ++= List(
      "io.circe" %%% "circe-generic" % "0.14.5",
      "io.circe" %%% "circe-parser"  % "0.14.5" % Test
    )
  )

lazy val cli = crossProject(runtimes: _*)
  .in(file("modules/cli"))
  .dependsOn(circe)
  .settings(commonSettings: _*)
  .settings(
    name := "hedgehogs",
    libraryDependencies ++= List(
      "com.monovore" %%% "decline" % "2.4.1",
      "io.circe" %%% "circe-parser"  % "0.14.5",
       "org.typelevel" %%% "cats-effect-std" % "3.5.0", 
       "org.typelevel" %%% "cats-effect" % "3.5.0", 
    )
  )
