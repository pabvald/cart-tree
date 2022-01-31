scalaVersion := "3.1.0"

lazy val myProject = project
   .settings(
       scalacOptions += "-Yrangepos"
   )

libraryDependencies ++= List(
    "org.apache.poi" % "poi" % "3.17",
    "org.apache.poi" % "poi-ooxml" % "3.17",
    "org.scalameta" %% "munit" % "0.7.29" % Test
)
testFrameworks += TestFramework("munit.Framework")
