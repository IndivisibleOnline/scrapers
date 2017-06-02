import Dependencies._


lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "org.indivisiblewestchester",
      scalaVersion := "2.11.8",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "IW",
    libraryDependencies += scalaTest % Test
    )

resolvers += "Spring Plugins" at "http://repo.spring.io/plugins-release/"

libraryDependencies += "com.fasterxml.jackson.core" % "jackson-databind" % "2.8.6"
libraryDependencies += "com.fasterxml.jackson.module" % "jackson-module-scala_2.11" % "2.8.6"
libraryDependencies += "org.apache.spark" % "spark-streaming-mqtt_2.11" % "1.5.1"
libraryDependencies += "org.eclipse.paho" % "org.eclipse.paho.client.mqttv3" % "1.0.1"
libraryDependencies += "org.jsoup" % "jsoup" % "1.10.2"
