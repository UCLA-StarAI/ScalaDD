lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "edu.ucla.cs.starai",
      scalaVersion := "2.12.2",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "ScalaDD",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % Test,
    libraryDependencies += "com.google.guava" % "guava" % "12.0",
    EclipseKeys.withSource := true,
    EclipseKeys.withJavadoc := true,
    EclipseKeys.withBundledScalaContainers := false
  )
