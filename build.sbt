
name := "apprenticeship-scorecard-api"

scalaVersion := "2.11.8"

lazy val `apprenticeship-scorecard-api` = (project in file("."))
  .enablePlugins(PlayScala)
  .disablePlugins(PlayLayoutPlugin)
  .enablePlugins(GitVersioning)
  .enablePlugins(GitBranchPrompt)

git.useGitDescribe := true

PlayKeys.devSettings := Seq("play.server.http.port" -> "9004")

routesImport ++= Seq("uk.gov.bis.apprenticeshipScorecard.models._", "uk.gov.bis.apprenticeshipScorecard.bindings._")
routesImport += "com.wellfactored.playbindings.ValueClassUrlBinders._"

libraryDependencies ++= Seq(
  ws,
  "joda-time" % "joda-time" % "2.7",
  "org.joda" % "joda-convert" % "1.7",
  "org.typelevel" %% "cats" % "0.6.1",
  "com.wellfactored" %% "restless" % "0.5.1",
  "com.wellfactored" %% "play-bindings" % "1.0.0",

  "org.scalatestplus.play" %% "scalatestplus-play" % "1.5.0" % Test
)

resolvers += Resolver.sonatypeRepo("releases")

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.7.1")
