package me.mycf.teatimer.terminal

import scala.annotation.tailrec
import scala.io.StdIn

object Main {
  def main(args: Array[String]): Unit = {
    if (args.isEmpty) {
      println("Wrong input, tea is missing!")
      System.exit(0)
    }

    brewTheTea(args, { () => true }, mapTeaToTime(args(0)))
  }


  def goodBye(): Unit = {
    println("I hope you enjoyed your Tea!")
    System.exit(0)
  }

  @tailrec
  final def brewTheTea(args: Array[String], continueBrewingSession: () => Boolean, lastRound: Option[Int] = Some(0)): Int = {
    if (!continueBrewingSession.apply()) goodBye() // exit
    val a: Int = if (args.isEmpty) {
      println("Wrong input, tea is missing!")
      System.exit(0)
      0
    }
    else (for {
      x <- args
      //      time <- lastRound
      y <- lastRound
    } yield timerIO(y)).array(0)

    brewTheTea(args, { () =>
      (for {
        x <- args
        y <- mapTeaToTime(x, a)
      } yield customReadBoolean(s"Brew the next infusion for ${y} seconds?")).array(0)
    }, {
      (for {
        x <- args
      } yield mapTeaToTime(x, a)).array(0)
    })
  }


  def mapTeaToTime(arg: String, lastRound: Int = 0): Option[Int] = {
    def isNumber(x: String) = x forall Character.isDigit

    if (!isNumber(arg)) arg.toLowerCase match {
      case "black" | "schwarz" | "puerhrr" | "puerh_ripe" => if (lastRound == 0) Some(10) else Some(lastRound + 5)
      case "puerhr" | "puerh_raw" => if (lastRound == 0) Some(10) else Some(lastRound + 3)
      case "green" | "gruen" | "grün" => if (lastRound == 0) Some(15) else Some(lastRound + 3)
      case "yellow" | "gelb" => if (lastRound == 0) Some(15) else Some(lastRound + 5)
      case "white" | "weiß" => if (lastRound == 0) Some(20) else Some(lastRound + 10)
      case "oolongs" | "oolong_strip" => if (lastRound == 0) Some(20) else Some(lastRound + 5)
      case "oolong" | "oolong_ball" => if (lastRound == 0) Some(25) else Some(lastRound + 5)
      case _ => {
        println("Wrong tea")
        System.exit(0)
        None
      }
    } else {
      println("Please enter a Tea...")
      System.exit(0)
      None
    }
  }


  def timerIO(y: Int): Int = {
    def announceSleepIO(x: Int): Int = {
      println(s"Brew the Tea for $x seconds!")
      x * 1000
    }

    Thread.sleep(announceSleepIO(y))
    Runtime.getRuntime.exec("say 'The Tea is done!'")
    y
  }

  def customReadBoolean(string: String): Boolean = {
    println(string)
    val s = StdIn.readLine()
    s.toLowerCase match {
      case "true" | "t" | "ja" | "j" | "yes" | "y" | "sure" | "yeah" => true
      case _ => false
    }
  }
}