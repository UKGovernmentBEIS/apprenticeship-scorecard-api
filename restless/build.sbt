
name := "restless"

scalaVersion := "2.11.8"

enablePlugins(GitVersioning)
enablePlugins(GitBranchPrompt)

git.useGitDescribe := true

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats" % "0.4.1",
  "com.chuusai" %% "shapeless" % "2.3.0",
  "org.tpolecat" %% "atto-core"  % "0.4.2",
  "org.scalatestplus.play" %% "scalatestplus-play" % "1.5.0-RC1" % Test
)


resolvers += Resolver.sonatypeRepo("releases")

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.7.1")
