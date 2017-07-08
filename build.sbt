lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "edu.ucla.cs.starai",
      scalaVersion := "2.12.2",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "ScalaDD",
    unmanagedSourceDirectories in Compile += baseDirectory.value / "src" / "scratch" / "scala",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % Test,
    libraryDependencies += "com.google.guava" % "guava" % "22.0",
    EclipseKeys.withSource := true,
    EclipseKeys.withJavadoc := true,
    organizationName := "Guy Van den Broeck <guyvdb@cs.ucla.edu>",
	startYear := Some(java.time.Year.now().getValue),
	licenses += ("Apache-2.0", new URL("https://www.apache.org/licenses/LICENSE-2.0.txt"))
  )
