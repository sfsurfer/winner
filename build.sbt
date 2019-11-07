name := "winner"
version := "0.0.1-SNAPSHOT"

scalaVersion := "2.12.7"

scalacOptions ++= Seq(
  "-encoding", "UTF-8",   // source files are in UTF-8
  "-deprecation",         // warn about use of deprecated APIs
  "-unchecked",           // warn about unchecked type parameters
  "-feature",             // warn about misused language features
  "-language:higherKinds",// allow higher kinded types without `import scala.language.higherKinds`
  "-Xlint",               // enable handy linter warnings
  "-Xfatal-warnings",     // turn compiler warnings into errors
  "-Ypartial-unification" // allow the compiler to unify type constructors of different arities
)

resolvers in ThisBuild += "Artima Maven Repository" at "http://repo.artima.com/releases"

libraryDependencies += "org.typelevel" %% "cats-core" % "1.4.0"
libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.8"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.8" % "test"

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.3")

