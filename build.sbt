import _root_.sbt.Keys._

lazy val generalSettings =
  Seq(
    name := "scala-course",
    version := "1.0",
    scalaVersion := "2.12.2",
    scalacOptions := Seq(
      "-encoding", "utf8",
      "-feature",
      "-unchecked",
      "-deprecation",
      "-target:jvm-1.8",
      "-Ymacro-debug-lite",
      "-language:experimental.macros"
    ),
    resolvers ++= Seq(
      Resolver.jcenterRepo,
      Resolver.url("jCenter", url("http://jcenter.bintray.com/")),
      Resolver.sonatypeRepo("releases"),
      Resolver.sonatypeRepo("snapshots")
    ),
    libraryDependencies ++= Seq(
      "com.typesafe.akka" %% "akka-actor" % "2.5.3",
      "com.typesafe.akka" %% "akka-testkit" % "2.5.3" % Test,
      "com.typesafe.akka" %% "akka-remote" % "2.5.3",
      "com.typesafe.akka" %% "akka-slf4j" % "2.5.3",
      "org.scala-lang" % "scala-library" % "2.12.2",
      "org.scala-lang" % "scala-reflect" % "2.12.2",
      "org.xerial" % "sqlite-jdbc" % "3.7.2",
      "org.scalatest" %% "scalatest" % "3.0.1" % "test",
      "org.mockito" % "mockito-core" % "1.9.5" % "test",
      "org.scalacheck" %% "scalacheck" % "1.13.4" % "test"
    )
  )

lazy val OopTest = config("oop") extend Test

def itFilter(name: String): Boolean = name startsWith "lectures.oop"

lazy val assemblySettings = Seq(
  assemblyOutputPath in assembly := file(s"target/scala-course-assembly-${version.value}.jar"),
  mainClass in assembly := Some("lectures.oop.TreeTest"),
  test in assembly := {}
)

lazy val root = (project in file("."))
  .configs(OopTest)
  .settings(
    generalSettings,
    inConfig(OopTest)(Defaults.testTasks),
    testOptions in OopTest := Seq(Tests.Filter(itFilter)),
    assemblySettings
  )


