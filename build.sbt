
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
  ws,
  "joda-time" % "joda-time" % "2.7",
  "org.joda" % "joda-convert" % "1.7",
  "org.typelevel" %% "cats" % "0.4.1",
  "com.wellfactored" %% "restless" % "0.1.2",
  "org.scalatestplus.play" %% "scalatestplus-play" % "1.5.0-RC1" % Test
)

resolvers += Resolver.sonatypeRepo("releases")

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.7.1")
