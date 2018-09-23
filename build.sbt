name := "Met-CS-810"

version := "1.0"

scalaVersion := "2.12.3"

libraryDependencies ++= Seq(
  "com.zenecture"   %%   "neuroflow-core"          %   "1.7.4",
  "com.zenecture"   %%   "neuroflow-application"   %   "1.7.4",
  "org.scalatest" % "scalatest_2.12" % "3.2.0-SNAP9" % "test",
  "org.apache.commons" % "commons-math3" % "3.6.1",
  "org.typelevel" %% "cats-core" % "1.1.0" ,
  "com.cra.figaro" %% "figaro" % "5.0.0.0"

)

resolvers ++= Seq("neuroflow-libs" at "https://github.com/zenecture/neuroflow-libs/raw/master/")

scalacOptions += "-Ypartial-unification"
