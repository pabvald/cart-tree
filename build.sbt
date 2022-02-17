scalaVersion := "3.1.0"

libraryDependencies ++= List(
    "org.scalameta" %% "munit-scalacheck" % "0.7.29" % Test
)
testFrameworks += TestFramework("munit.Framework")
