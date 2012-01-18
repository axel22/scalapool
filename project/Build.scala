/*    ____          __                   __                            *\
**   / __/_______ _/ /___ ____ ___ ___  / /                            **
**  _\ \ / __/ _ `/ // _ `/ _ | _ | _ \/ /   Memory Pool API           **
** /___/ \__/\_,_/_/ \_,_/ .__|___|___/_/    github.com/axel22/mempool **
\*                      /_/                  (c) 2011-2012             */



import sbt._
import Keys._
import Process._
import java.io.File



object ScalapoolBuild extends Build {
  
  /* tasks and settings */
  
  val benchTask = InputKey[Unit](
    "bench",
    "Runs a specified benchmark."
  ) <<= inputTask {
    (argTask: TaskKey[Seq[String]]) =>
      (argTask,
       dependencyClasspath in Compile,
       artifactPath in (Compile, packageBin),
       artifactPath in (Test, packageBin),
       packageBin in Compile,
       packageBin in Test) map {
      (args, dp, jar, testjar, pbc, pbt) =>
      val javacommand = "java -XX:+UnlockDiagnosticVMOptions -XX:+PrintAssembly -verbose:gc -Xprof -Xmx512m -Xms512m -server -cp %s:%s:%s".format(
        dp.map(_.data).mkString(":"),
        jar,
        testjar
      )
      val comm = javacommand + " " + args.mkString(" ")
      comm!
    }
  }
  
  
  /* projects */
  
  lazy val storm = Project(
    "scalapool",
    file("."),
    settings = Defaults.defaultSettings ++ Seq(benchTask)
  ) dependsOn (
  )
  
}


