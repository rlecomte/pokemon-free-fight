name          := "pkm-ff"
version       := "0.0.1"

scalaVersion  := "2.11.8"

scalacOptions ++= Seq(
  "-deprecation",           // Warn when deprecated API are used
  "-feature",               // Warn for usages of features that should be importer explicitly
  "-unchecked",             // Warn when generated code depends on assumptions
  "-Xlint",                 // Additional warnings (see scalac -Xlint:help)
  "-Xlog-reflective-calls", // Print a message when a reflective method call is generated
  "-Ywarn-numeric-widen",   // Warn when numeric are widened
  "-Ywarn-dead-code",       // Warn when dead code is identified
  "-Ywarn-adapted-args",    // Warn if an argument list is modified to match the receive
  "-Ywarn-inaccessible",    // Warn about inaccessible types in method signatures
  "-Ywarn-nullary-override",// Warn when non-nullary overrides nullary, e.g. def foo() over def foo.
  "-Ywarn-infer-any"        // Warn when a type argument is inferred to be `Any`.
)

resolvers += Resolver.jcenterRepo

libraryDependencies in ThisBuild ++= {
  val akkaV = "2.4.9-RC2"
  val akkaCirceV = "1.7.0"
  val circeV = "0.4.1"
  val catsV = "0.5.0"
  Seq(
    "com.typesafe.akka" %% "akka-actor" % akkaV,
    "com.typesafe.akka" %% "akka-slf4j"  % akkaV,
    "com.typesafe.akka" %% "akka-http-core" % akkaV,
    "com.typesafe.akka" %% "akka-http-experimental" % akkaV,
    "com.typesafe.akka" %% "akka-testkit" % akkaV,
    "com.typesafe.akka" %% "akka-http-testkit" % akkaV, /*% "test" */
    "de.heikoseeberger" %% "akka-http-circe" % akkaCirceV,

    "io.circe" %% "circe-generic" % circeV,

    "org.typelevel" %% "cats" % catsV,

    "ch.qos.logback" % "logback-classic" % "1.1.7",
    "com.typesafe.scala-logging" %% "scala-logging" % "3.4.0",

    "org.scalatest" %% "scalatest" % "2.2.6" % "test",
    "org.scalamock" %% "scalamock-scalatest-support" % "3.2.2" % "test",
    "org.scalacheck" %% "scalacheck" % "1.12.5" % "test")
}
