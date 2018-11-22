name := "Met-CS-810"

version := "1.0"

scalaVersion := "2.12.7"

libraryDependencies ++= Seq(
  "com.zenecture"   %%   "neuroflow-core"          %   "1.7.4",
  "com.zenecture"   %%   "neuroflow-application"   %   "1.7.4",
  "org.scalatest" % "scalatest_2.12" % "3.2.0-SNAP9" % "test",
  "org.apache.commons" % "commons-math3" % "3.6.1",
  "com.cra.figaro" %% "figaro" % "5.0.0.0",
  "io.argonaut" %% "argonaut" % "6.2.2",
  "com.typesafe.play" %% "play-json" % "2.6.10" ,
  "org.scalaz" %% "scalaz-core" % "7.3.0-M26"
)

dependencyOverrides ++= Set(
  "io.argonaut" %% "argonaut" % "6.2.2"
)

resolvers ++= Seq("neuroflow-libs" at "https://github.com/zenecture/neuroflow-libs/raw/master/")

scalacOptions += "-Ypartial-unification"
