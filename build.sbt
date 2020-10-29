import sbt.Keys.{scalacOptions, _}

enablePlugins()

lazy val root = (project in file("."))
  .enablePlugins(PlayScala, PlayLayoutPlugin, JavaAgentPackaging)
  .settings(
    name := """car_registry""",
    scalaVersion := "2.13.3",
    libraryDependencies ++= Seq(
      guice,
      "net.logstash.logback" % "logstash-logback-encoder" % "6.2",
      "org.scalatestplus.play" %% "scalatestplus-play" % "5.0.0" % Test,
      "io.lemonlabs" %% "scala-uri" % "1.5.1",
      "net.codingwell" %% "scala-guice" % "4.2.6"
    ),
    scalacOptions ++= Seq(
      "-feature",
      "-deprecation",
      "-Xfatal-warnings"
    )
  )


// Adds additional packages into Twirl
//TwirlKeys.templateImports += "com.example.controllers._"

// Adds additional packages into conf/routes
// play.sbt.routes.RoutesKeys.routesImport += "com.example.binders._"
