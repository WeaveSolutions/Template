name := """nexpo-api-java"""
organization := "com.nexpo"

version := "1.0.0"

lazy val root = (project in file(".")).enablePlugins(PlayJava)

scalaVersion := "2.13.12"

libraryDependencies ++= Seq(
  guice,
  ws,
  
  // Auth0 JWT validation
  "com.auth0" % "java-jwt" % "4.4.0",
  "com.auth0" % "jwks-rsa" % "0.22.0",
  
  // JSON processing
  "com.fasterxml.jackson.core" % "jackson-databind" % "2.15.2",
  "com.fasterxml.jackson.datatype" % "jackson-datatype-jsr310" % "2.15.2",
  
  // Swagger/OpenAPI
  "io.swagger" % "swagger-play2_2.13" % "2.0.0-SNAPSHOT",
  "io.swagger" % "swagger-annotations" % "1.6.11",
  "io.swagger" % "swagger-core" % "1.6.11",
  "org.webjars" % "swagger-ui" % "5.7.0",
  
  // Logging
  "net.logstash.logback" % "logstash-logback-encoder" % "7.4",
  
  // Testing
  "org.assertj" % "assertj-core" % "3.24.2" % Test,
  "org.mockito" % "mockito-core" % "5.4.0" % Test
)

// Play settings
PlayKeys.playDefaultPort := 8070

// Java compilation options
javacOptions ++= Seq(
  "-encoding", "UTF-8",
  "-parameters",
  "-Xlint:unchecked",
  "-Xlint:deprecation"
)

// Test settings
Test / testOptions += Tests.Argument(TestFrameworks.JUnit, "-a", "-v")

// Docker settings
Docker / maintainer := "nexpo@example.com"
Docker / packageName := "nexpo-api-java"
Docker / version := version.value
Docker / dockerExposedPorts := Seq(8070)
Docker / dockerBaseImage := "openjdk:11-jre-slim"
Docker / dockerRepository := Some("nexpo")

// JVM options
Universal / javaOptions ++= Seq(
  "-Dpidfile.path=/dev/null",
  "-Dplay.http.secret.key=${?PLAY_HTTP_SECRET_KEY}"
)
