import sbtwelcome._

val runtimes       = List(JVMPlatform, JSPlatform)
val scalaVersions  = List("2.13.18", "3.3.7")
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
  version                    := "0.5.0",
  scalaVersion               := "3.3.7",
  ThisBuild / scalafmtConfig := file(".scalafmt.conf"),
  useGpg                     := true,
  pomIncludeRepository       := { _ => false },
  publishMavenStyle          := true,
  publishTo                  := {
    val centralSnapshots = "https://central.sonatype.com/repository/maven-snapshots/"
    if (isSnapshot.value) Some("central-snapshots" at centralSnapshots)
    else localStaging.value
  },
  licenses                   := Seq("MIT" -> url("https://opensource.org/licenses/MIT")),
  scmInfo                    := Some(
    ScmInfo(url("https://github.com/andimiller/hedgehogs"), "scm:git@github.com:andimiller/hedgehogs.git")
  ),
  homepage                   := Some(url("https://github.com/andimiller/hedgehogs")),
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
  .aggregate(
    core.js,
    core.jvm,
    mermaid.js,
    mermaid.jvm,
    `dag-visitor`.js,
    `dag-visitor`.jvm,
    `dag-visitor-circe`.js,
    `dag-visitor-circe`.jvm,
    exampleSuspendableHttp
  )
  .settings(commonSettings)
  .settings(
    crossScalaVersions := Nil,
    publish / skip     := true,
    sonaDeploymentName := s"hedgehogs-${version.value}"
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

lazy val `dag-visitor` = crossProject(runtimes: _*)
  .in(file("modules/dag-visitor"))
  .dependsOn(core)
  .settings(commonSettings: _*)
  .settings(
    name := "hedgehogs-dag-visitor",
    libraryDependencies ++= List(
      "org.typelevel" %%% "cats-effect"         % "3.6.3",
      "org.typelevel" %%% "cats-effect-testkit" % "3.6.3" % Test
    )
  )

lazy val `dag-visitor-circe` = crossProject(runtimes: _*)
  .in(file("modules/dag-visitor-circe"))
  .dependsOn(`dag-visitor`)
  .settings(commonSettings: _*)
  .settings(
    name := "hedgehogs-dag-visitor-circe",
    libraryDependencies ++= List(
      "io.circe" %%% "circe-core"   % "0.14.16",
      "io.circe" %%% "circe-parser" % "0.14.16" % Test
    )
  )

// runnable demo of suspending a dag to disk and resuming it over http, not published
lazy val exampleSuspendableHttp = (project in file("examples/suspendable-http"))
  .dependsOn(`dag-visitor-circe`.jvm)
  .settings(
    name               := "hedgehogs-example-suspendable-http",
    organization       := "net.andimiller",
    scalaVersion       := "3.3.7",
    crossScalaVersions := scalaVersions,
    publish / skip     := true,
    libraryDependencies ++= List(
      "org.http4s" %% "http4s-ember-server" % "0.23.30",
      "org.http4s" %% "http4s-dsl"          % "0.23.30",
      "org.http4s" %% "http4s-circe"        % "0.23.30",
      "io.circe"   %% "circe-parser"        % "0.14.16"
    )
  )

