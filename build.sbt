/*
 * Copyright 2016-2019 Daniel Urban and contributors listed in AUTHORS
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

lazy val scates = project.in(file("."))
  .settings(name := "scates")
  .settings(commonSettings)
  .configs(IntegrationTest) // We're abusing IntegrationTest test for separating
  .settings(Defaults.itSettings) // sources which take a long time to compile.

lazy val commonSettings = Seq[Setting[_]](
  scalaVersion := "2.12.4-bin-typelevel-4",
  crossScalaVersions := Seq(scalaVersion.value, "2.11.11-bin-typelevel-4"),
  scalaOrganization := "org.typelevel",
  scalacOptions ++= Seq(
    "-feature",
    "-deprecation",
    "-unchecked",
    "-encoding", "UTF-8",
    "-language:higherKinds,existentials",
    "-Xlint:_",
    "-Xfuture",
    "-Xfatal-warnings",
    "-Yno-adapted-args",
    "-Ywarn-numeric-widen",
    // "-Ywarn-dead-code", // we want to use ??? in examples
    "-Ypartial-unification",
    "-Ywarn-unused-import"
  ),
  scalacOptions in (Compile, console) ~= { _.filterNot("-Ywarn-unused-import" == _) },
  scalacOptions in (Test, console) := (scalacOptions in (Compile, console)).value,
  addCompilerPlugin("org.spire-math" % "kind-projector" % "0.9.5" cross CrossVersion.binary),

  // We need both of these, due to https://github.com/scalastyle/scalastyle-sbt-plugin/issues/44
  scalastyleConfig in Test := (baseDirectory in ThisBuild).value / "scalastyle-test-config.xml",
  scalastyleConfig in scalastyle := (baseDirectory in ThisBuild).value / "scalastyle-test-config.xml",

  libraryDependencies ++= Seq(
    dependencies.cats,
    dependencies.fs2,
    dependencies.akka,
    Seq(
      dependencies.shapeless
    ),
    dependencies.test.map(_ % "test-internal")
  ).flatten,
  organization := "io.sigs",
  publishMavenStyle := true,
  publishArtifact := false, // TODO,
  licenses := Seq("Apache 2.0" -> url("https://www.apache.org/licenses/LICENSE-2.0.txt"))
)

lazy val dependencies = new {

  val catsVersion = "1.1.0"
  val fs2Version = "0.10.4"

  val shapeless = "com.chuusai" %% "shapeless" % "2.3.3"
  val cats = Seq(
    "org.typelevel" %% "cats-core" % catsVersion,
    "org.typelevel" %% "cats-free" % catsVersion,
    "org.typelevel" %% "cats-effect" % "1.0.0-RC"
  )

  val fs2 = Seq(
    "co.fs2" %% "fs2-core" % fs2Version,
    "co.fs2" %% "fs2-io" % fs2Version
  )

  val laws = Seq(
    "org.typelevel" %% "cats-laws" % catsVersion,
    "com.github.alexarchambault" %% "scalacheck-shapeless_1.13" % "1.1.6"
  )

  val test = Seq(
    "org.scalatest" %% "scalatest" % "3.0.5"
  )

  val akka = Seq(
    "com.typesafe.akka" %% "akka-actor-typed" % "2.5.12"
  )
}
