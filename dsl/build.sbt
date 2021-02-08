val dottyVersion = "0.27.0-RC1"

val alpacasVersion = "0.1.0"

lazy val root = project
  .in(file("."))
  .settings(
    name := "alpacas",
    version := alpacasVersion,
    scalaVersion := dottyVersion,
    libraryDependencies += "org.scala-graph" %% "graph-core" % "1.13.2",
    libraryDependencies += "org.typelevel" %% "cats-core" % "2.0.0",
    libraryDependencies += "org.apache.commons" % "commons-math3" % "3.6",
    libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.3.2",
    libraryDependencies += "org.scala-lang.modules" %% "scala-parallel-collections" % "0.2.0",
    libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.14.1" % "test",
    libraryDependencies := libraryDependencies.value.map(_.withDottyCompat(scalaVersion.value)),

    libraryDependencies += "com.lihaoyi" %% "sourcecode" % "0.2.1",
    libraryDependencies += "org.scalatest" %% "scalatest-funsuite" % "3.2.2" % "test",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.2" % "test",

    assemblyJarName in assembly := s"fat-alpacas_$dottyVersion-$alpacasVersion.jar",

    fork in (Compile, run) := false, 
    fork in (Test, run) := false, 
    fork in (Test, test) := false,
    Test / testForkedParallel := false,      
    javaOptions += "-Xmx6G",
  )

enablePlugins(JavaAppPackaging)
enablePlugins(GraalVMNativeImagePlugin)
