
name := "apprenticeship-scorecard-api"

scalaVersion := "2.11.8"

enablePlugins(PlayScala)
disablePlugins(PlayLayoutPlugin)

enablePlugins(GitVersioning)
enablePlugins(GitBranchPrompt)

git.useGitDescribe := true

PlayKeys.devSettings := Seq("play.server.http.port" -> "9004")

routesImport ++= Seq("apprenticeshipScorecard.bindings._", "com.wellfactored.restless.QueryAST.Query")

libraryDependencies ++= Seq(
  cache,
  ws,
  "joda-time" % "joda-time" % "2.7",
  "org.joda" % "joda-convert" % "1.7",
  "com.typesafe.play" %% "play-slick" % "2.0.0",
  "com.typesafe.play" %% "play-slick-evolutions" % "2.0.0",
  "com.h2database" % "h2" % "1.4.191",
  "org.postgresql" % "postgresql" % "9.4.1208",
  "org.typelevel" %% "cats" % "0.4.1",
  "com.chuusai" %% "shapeless" % "2.3.0",
  "org.tpolecat" %% "atto-core" % "0.4.2",
  "org.scalatestplus.play" %% "scalatestplus-play" % "1.5.0-RC1" % Test
)

lazy val restless = project.in(file("restless"))

lazy val root = project.in(file(".")).dependsOn(restless).aggregate(restless)

resolvers += Resolver.sonatypeRepo("releases")

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.7.1")
