
name := "restless-play-json"

scalaVersion := "2.11.8"

enablePlugins(GitVersioning)
enablePlugins(GitBranchPrompt)

git.useGitDescribe := true

libraryDependencies ++= Seq(
  "com.typesafe.play" %% "play-json" % "2.5.3",
  "org.scalatest" %% "scalatest" % "2.2.6" % "test"
)

