lazy val root = (project in file(".")).
  settings(
    name := "hexagon signal",
    version := "0.0.0",
    scalaVersion := "2.12.1",
    libraryDependencies += "org.scalafx" %% "scalafx" % "8.0.102-R11",
    libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.4.16",
    libraryDependencies += "de.sciss" %% "scalacollider" % "1.22.3",
    libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.5",
    fork := true
  )
