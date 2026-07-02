import com.typesafe.sbt.packager.docker.Cmd
import org.scalajs.linker.interface.ESVersion
import org.scalajs.sbtplugin.ScalaJSPlugin
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport._
import sbtdynver.DynVer

ThisBuild / organization := "samson.ph"
ThisBuild / scalaVersion := "3.8.4"
ThisBuild / versionScheme := Some("semver-spec")

lazy val root = Project("atbp", file("."))
  .settings(
    name := "atbp-root",
    publish / skip := true
  )
  .aggregate(
    cli,
    confluence,
    http,
    hubad,
    jira,
    liga,
    ligaJs,
    md2c,
    plate,
    retext,
    stmt2csv,
    stmt2csvIt,
    traceviz
  )

lazy val cli = atbpModule("cli")
  .dependsOn(hubad, liga, md2c, plate, retext, stmt2csv, traceviz)
  .enablePlugins(
    DockerPlugin,
    JavaAppPackaging
  )
  .settings(Dependencies.cli)
  .settings(
    // sbt-native-packager
    packageName := "atbp",
    maintainer := "edward@samson.ph",
    executableScriptName := packageName.value,
    dockerBaseImage := "eclipse-temurin:21-jre-noble",
    dockerRepository := Some("ghcr.io/esamson"),
    Docker / version := (version.value)
      .replace("+", ".")
      .replace("-", ".")
      .toLowerCase,
    dockerUpdateLatest := !DynVer.isSnapshot(),
    dockerAliases ++= {
      if (DynVer.isSnapshot()) {
        Seq(dockerAlias.value.withTag(Some("snapshot")))
      } else {
        Nil
      }
    },
    dockerCommands ++= Seq(
      Cmd("USER", "root"),
      Cmd(
        "RUN",
        "apt-get update",
        "&& apt-get install -y git graphviz",
        "&& apt-get autoremove",
        "&& apt-get clean",
        "&& rm -rf /var/lib/apt/lists/*"
      )
    )
  )

lazy val confluence = atbpModule("confluence")
  .dependsOn(adfBuilder, http)
  .settings(Dependencies.confluence)

lazy val http = atbpModule("http")
  .settings(Dependencies.http)

lazy val jira = atbpModule("jira")
  .dependsOn(http)
  .settings(Dependencies.jira)

lazy val md2c = atbpModule("md2c")
  .dependsOn(adfBuilder, confluence)
  .settings(Dependencies.md2c)
  .settings(
    // Don't exclude hidden files so .md2c.conf files show up in tests.
    Test / fileInputExcludeFilter := DirectoryFilter.toNio
  )

lazy val plate = atbpModule("plate")
  .dependsOn(jira)
  .settings(Dependencies.plate)

lazy val retext = atbpModule("retext")
  .settings(Dependencies.retext)

lazy val stmt2csv = atbpModule("stmt2csv")
  .settings(Dependencies.stmt2csv)

lazy val stmt2csvIt = atbpModule("stmt2csv-it")
  .dependsOn(stmt2csv)
  .settings(Dependencies.stmt2csvIt)

lazy val traceviz = atbpModule("traceviz")
  .settings(Dependencies.traceviz)

lazy val hubad = atbpModule("hubad")
  .settings(Dependencies.hubad)

lazy val liga = atbpModule("liga")
  .dependsOn(http)
  .settings(Dependencies.liga)

lazy val ligaJs = atbpModule("liga-js")
  .enablePlugins(ScalaJSPlugin)
  .settings(
    Compile / packageDoc / mappings := Nil,
    scalaJSUseMainModuleInitializer := false,
    scalaJSLinkerConfig :=
      scalaJSLinkerConfig.value
        .withESFeatures(_.withESVersion(ESVersion.ES2017)),
    libraryDependencies ++= Seq(
      "com.github.mrdimosthenis" %%% "glicko2" % "1.0.1",
      "com.raquo" %%% "laminar" % "17.2.1"
    )
  )

// Pseudo-project to limit usage of Atlassian repo
lazy val adfBuilder = atbpModule("adf-builder")
  .settings(
    resolvers += "Atlassian Packages" at "https://packages.atlassian.com/artifactory/maven-external",
    Dependencies.adfBuilder
  )

def atbpModule(moduleName: String): Project =
  Project(moduleName, file(moduleName))
    .settings(name := s"atbp-$moduleName")
    .settings(
      Compile / packageDoc / mappings := Nil,
      Compile / run / fork := true,
      Test / run / fork := true,
      Test / testForkedParallel := true
    )
    .settings(scalacOptions ++= Seq("-no-indent", "-old-syntax"))
