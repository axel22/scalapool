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
  
  /* tasks */
  
  val benchTask = InputKey[Unit](
    "bench",
    "Runs a specified benchmark."
  ) <<= inputTask {
    (argTask: TaskKey[Seq[String]]) =>
    (argTask, scalaInstance) map {
      (args, scala) =>
      val javacommand = "java -Xmx2048m -Xms2048m -server -cp %s".format(
        scala.libraryJar
      )
      println(javacommand + " " + args.mkString(" "));
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


