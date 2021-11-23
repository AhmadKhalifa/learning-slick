name := "Learning Slick"

version := "0.1"

scalaVersion := "2.13.7"

idePackagePrefix := Some("com.slick.learning")

libraryDependencies ++= Seq(
  "com.typesafe.slick" %% "slick" % "3.3.3",
  "org.slf4j" % "slf4j-nop" % "1.7.32",
  "com.h2database" % "h2" % "1.4.185",
)
