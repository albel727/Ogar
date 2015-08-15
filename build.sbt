name := "scala-agario-server"

version := "1.0"

scalaVersion := "2.11.7"

scalacOptions := Seq("-unchecked", "-deprecation", "-encoding", "utf8") //, "-Ylog-classpath"

mainClass in Compile := Some("com.agariomods.ogar.OgarApp")

resolvers += "Spray" at "http://repo.spray.io"

libraryDependencies += "com.wandoulabs.akka" %% "spray-websocket" % "0.1.4"

libraryDependencies += "io.spray" %% "spray-json" % "1.3.2"

libraryDependencies += "org.clapper" %% "grizzled-scala" % "1.3"

libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value

libraryDependencies += "jline" % "jline" % "2.12"

libraryDependencies += "com.typesafe.akka" %% "akka-slf4j" % "2.3.12"

libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.1.3"
