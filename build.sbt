name := "cats"

version := "0.1"

scalaVersion := "3.1.1"

val catsVersion = "2.10.0"

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % catsVersion,
)

scalacOptions ++= Seq(
  "-language:higherKinds"
)