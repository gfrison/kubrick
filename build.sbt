val scalaV = "3.7.4"
ThisBuild / organization := "kubrick"
ThisBuild / version      := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion := scalaV
ThisBuild / headerLicense := Some(HeaderLicense.MIT("2025", "Giancarlo Frison"))
val scalaTest = "3.2.19"
val sharedSettings = Seq(
  scalaVersion := scalaV,
  libraryDependencies ++= Seq(
    "com.outr"               %%% "scribe"                   % "3.17.0",
    "org.typelevel"          %%% "cats-core"                % "2.13.0",
    "org.typelevel"          %%% "cats-effect"              % "3.6.3",
    "org.typelevel"          %%% "literally"                % "1.2.0",
    "com.lihaoyi"            %%% "fastparse"                % "3.1.1",
    "org.scala-lang.modules" %%% "scala-collection-contrib" % "0.4.0",
    "co.fs2"                 %%% "fs2-core"                 % "3.12.2"
  )
)
val jvmSettings = Seq(
  libraryDependencies ++= Seq(
    "org.scalatest" %%% "scalatest"                     % scalaTest % Test,
    "org.typelevel" %%% "cats-effect-testing-scalatest" % "1.7.0"   % Test
  )
)
val jsSettings = Seq(
  testFrameworks += new TestFramework("munit.Framework"),
  scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.CommonJSModule) },
  semanticdbEnabled := true,
  semanticdbVersion := scalafixSemanticdb.revision,
  autoAPIMappings   := true,
  libraryDependencies ++= Seq(
    "org.scalameta" %%% "munit" % "1.2.1" % Test
  )
)
lazy val prelude = crossProject(JSPlatform, JVMPlatform)
  .withoutSuffixFor(JVMPlatform)
  .in(file("prelude"))
  .settings(sharedSettings)
  .jvmSettings(jvmSettings)
  .jsSettings(jsSettings)
lazy val kube = crossProject(JSPlatform, JVMPlatform)
  .withoutSuffixFor(JVMPlatform)
  .in(file("kube"))
  .jsConfigure(_.dependsOn(prelude.js))
  .jvmConfigure(_.dependsOn(prelude.jvm))
  .settings(sharedSettings)
  .jvmSettings(jvmSettings)
  .jsSettings(jsSettings)
inThisBuild(
  List(
    scalaVersion      := scalaV,
    semanticdbEnabled := true,
    semanticdbVersion := scalafixSemanticdb.revision,
    scalacOptions ++= List(
      "-Wunused:unsafe-warn-patvars",
      "-Wunused:imports",
      "-source:future",
      "-language:experimental.modularity"
    )
  )
)
addCommandAlias("reorder", "scalafixAll;scalafmtAll;test")
