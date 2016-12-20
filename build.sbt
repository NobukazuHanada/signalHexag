lazy val root = (project in file(".")).
  settings(
    name := "hexagon signal",
    version := "0.0.0",
    scalaVersion := "2.11.8",
    libraryDependencies += "org.scalafx" %% "scalafx" % "8.0.102-R11",
    libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.4.11",
    fork := true
  )
