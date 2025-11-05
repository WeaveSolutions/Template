// The Play plugin
addSbtPlugin("com.typesafe.play" % "sbt-plugin" % "2.9.0")

// Defines scaffolding (found under .g8 folder)
// http://www.foundweekends.org/giter8/scaffolding.html
addSbtPlugin("org.foundweekends.giter8" % "sbt-giter8-scaffold" % "0.13.1")

// Docker plugin
addSbtPlugin("com.github.sbt" % "sbt-native-packager" % "1.9.16")
