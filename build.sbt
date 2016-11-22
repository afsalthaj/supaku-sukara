scalaVersion := "2.11.8"

resolvers += Resolver.sonatypeRepo("releases")

addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)
addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.3")

libraryDependencies ++= Seq (
  "org.scalatest" %% "scalatest" % "3.0.0" % "test",
  "org.scalacheck" %% "scalacheck" % "1.13.3" % "test",
  "org.specs2" %% "specs2-core" % "2.4.17" % "test",
  "org.specs2" %% "specs2-junit" % "2.4.17" % "test",
  "io.argonaut" %% "argonaut" % "6.1",
  "ml.sparkling" %% "sparkling-graph-examples" % "0.0.6",
  "ml.sparkling" %% "sparkling-graph-loaders" % "0.0.6",
  "ml.sparkling" %% "sparkling-graph-operators" % "0.0.6",
  "com.github.mpilquist" %% "simulacrum" % "0.10.0"
)