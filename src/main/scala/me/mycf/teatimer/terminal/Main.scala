package me.mycf.teatimer.terminal

import scala.io.StdIn

object Main {
  def main(args: Array[String]): Unit = {
    if !customReadBoolean("Would you like to start now?")
    then
      untilNextTime()
    else
      decidingWhatToDo(args)
  }

  def startAllOverAgain(): Unit = {
    println(
      """
        |The following teas are available:
        |kind           gr/100ml  time      temp
        |white          3.5/4gr   20/+10s    85C
        |green          3/3.5gr   15/+3s     80C
        |yellow         3.5/4gr   15/+5s     85C
        |oolong_strip   4.5/5gr   20/+5s     99C
        |oolong_ball    6/6.5gr   25/+5s     99C
        |black          4/4.5gr   10-15/+5s  99C
        |puerh_ripe     5gr       10/+3s     99C
        |puerh_raw      5gr       10/+5s     99C
        |custom         Enter first first-infusion time and then the added time per infusion.
        |
        |Keep in mind that those values are made for 功夫 style brewing!
        |""".stripMargin)

    decidingWhatToDo(StdIn.readLine().split(" "))
  }

  def decidingWhatToDo(args: Array[String]): Unit = {
    args.size match {
      case 1 => if args(0).toLowerCase == "-teas" then startAllOverAgain() else brewTheTea(args(0), continueAndLastRound = { () => 0 })
      case 3 => brewTheCustomTea(args(0), args(1), args(2), continueAndLastRound = { () => 0 })
      case _ => goodBye()
    }
  }

  def goodBye(numOrTea: String = "tea"): Unit = {
    println(s"Please enter a valid $numOrTea. Run with -teas for an overview.")
    System.exit(0)
  }

  def untilNextTime(): Unit = {
    println("I hope you enjoyed your Tea!")
    System.exit(0)
  }

  def untilNextTime0(): Int = {
    untilNextTime();
    0
  }

  def brewTheTea(tea: String, continueAndLastRound: () => Int): Unit = {
    val oldTime = mapTeaToTime(tea, lastRound = continueAndLastRound())
    timerIO(oldTime)

    brewTheTea(tea, { () =>
      if customReadBoolean(s"Would you like to continue brewing for ${mapTeaToTime(tea, lastRound = oldTime)} seconds?")
      then oldTime else untilNextTime0()
    })
  }

  def brewTheCustomTea(tea: String, customTime: String, customIncrease: String, continueAndLastRound: () => Int): Unit = {
    val oldTime = mapTeaToTime(tea, customTime = customTime.toInt, customIncrease = customIncrease.toInt, lastRound = continueAndLastRound())
    timerIO(oldTime)

    brewTheCustomTea(tea, customTime, customIncrease, { () =>
      if customReadBoolean(s"Would you like to continue brewing for ${mapTeaToTime(tea, lastRound = oldTime, customTime = customTime.toInt, customIncrease = customIncrease.toInt)} seconds?")
      then oldTime else untilNextTime0()
    })
  }

  def mapTeaToTime(tea: String, lastRound: Int = 0, customTime: Int = 0, customIncrease: Int = 0): Int = {
    def customLogic(): Int = {
      if customTime == 0 then goodBye()
      if lastRound == 0 then customTime else lastRound + customIncrease
    }

    tea.toLowerCase match {
      case "black" | "schwarz" | "puerhrr" | "puerh_ripe" => if lastRound == 0 then 10 else lastRound + 5
      case "puerhr" | "puerh_raw" => if lastRound == 0 then 10 else lastRound + 3
      case "green" | "gruen" | "grün" => if lastRound == 0 then 15 else lastRound + 3
      case "yellow" | "gelb" => if lastRound == 0 then 15 else lastRound + 5
      case "white" | "weiß" => if lastRound == 0 then 20 else lastRound + 10
      case "oolongs" | "oolong_strip" => if lastRound == 0 then 20 else lastRound + 5
      case "oolong" | "oolong_ball" => if lastRound == 0 then 25 else lastRound + 5
      case "custom" | "eigen" => customLogic()
      case _ => {
        goodBye()
        0
      }
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
