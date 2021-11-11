name := "scala-training"

version := "0.1"

val catsVersion = "2.5.0"
val catsTaglessVersion = "0.11"
val catsEffectVersion = "2.2.0"

scalaVersion := "2.13.6"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.2.9" % Test,
  "org.scalatestplus" %% "scalacheck-1-15" % "3.2.9.0" % Test,
  "org.typelevel" %% "cats-core" % catsVersion,
  "org.typelevel" %% "cats-effect" % catsEffectVersion,
)
