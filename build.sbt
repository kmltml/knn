ThisBuild / scalaVersion     := "2.12.8"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "com.example"
ThisBuild / organizationName := "example"

lazy val root = (project in file("."))
  .settings(
    name := "knn",
    libraryDependencies ++= Seq(
      "com.github.scopt" %% "scopt" % "4.0.0-RC2",
      "com.nrinaudo" %% "kantan.csv" % "0.4.0",
      "org.apache.spark" %% "spark-core" % "2.4.3",
      "org.apache.spark" %% "spark-mllib" % "2.4.3"
    )
  )
