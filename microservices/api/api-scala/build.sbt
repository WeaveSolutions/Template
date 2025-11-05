name := "nexpo-api-scala"
organization := "com.nexpo"
version := "1.0.0"

lazy val root = (project in file("."))
  .enablePlugins(PlayScala)

scalaVersion := "2.13.12"

libraryDependencies ++= Seq(
  guice,
  ws,
  caffeine,
  "org.scalatestplus.play" %% "scalatestplus-play" % "5.1.0" % Test,
  
  // JWT and Auth0
  "com.auth0" % "java-jwt" % "4.4.0",
  "com.auth0" % "jwks-rsa" % "0.22.1",
  
  // JSON processing
  "com.typesafe.play" %% "play-json" % "2.10.0",
  
  // Swagger/OpenAPI
  "io.swagger" %% "swagger-play2" % "1.8.0",
  "org.webjars" % "swagger-ui" % "5.9.0",
  
  // Logging
  "ch.qos.logback" % "logback-classic" % "1.4.11",
  "net.logstash.logback" % "logstash-logback-encoder" % "7.4",
  
  // Configuration
  "com.typesafe" % "config" % "1.4.3"
)

// Play configuration
PlayKeys.devSettings += "play.server.http.port" -> "8060"
PlayKeys.devSettings += "play.server.http.address" -> "0.0.0.0"

// Compile options
scalacOptions ++= Seq(
  "-feature",
  "-deprecation",
  "-unchecked",
  "-Xlint",
  "-Ywarn-dead-code",
  "-Ywarn-numeric-widen",
  "-Ywarn-value-discard"
)

// Test options
Test / testOptions += Tests.Argument(TestFrameworks.ScalaTest, "-oDF")

// Docker configuration
dockerExposedPorts := Seq(8060)
dockerBaseImage := "openjdk:11-jre-slim"
dockerUsername := Some("nexpo")
dockerRepository := Some("docker.io")
