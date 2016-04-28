
name := "restless-core"

scalaVersion := "2.11.8"

enablePlugins(GitVersioning)
enablePlugins(GitBranchPrompt)

git.useGitDescribe := true

libraryDependencies ++= Seq(
  "org.tpolecat" %% "atto-core" % "0.4.2",
  "org.scalatest" %% "scalatest" % "2.2.6" % "test"
)

