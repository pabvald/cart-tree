scalaVersion := "3.1.0"
libraryDependencies ++= List(
    "org.scalameta" %% "munit" % "0.7.29" % Test
)
testFrameworks += TestFramework("munit.Framework")

lazy val myProject = project
   .settings(
       scalacOptions += "-Yrangepos"
   )