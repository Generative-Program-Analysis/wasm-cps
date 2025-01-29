val scala3Version = "3.6.3"

lazy val root = project
  .in(file("."))
  .settings(
    name := "wasm-cps",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test",
    libraryDependencies += "org.scala-lang" %% "scala3-staging" % scalaVersion.value,
    libraryDependencies += "org.antlr" % "antlr4-runtime" % "4.13.0"
  )

scalacOptions ++= Seq(
  "-Xcheck-macros",
  "-explain"
)
