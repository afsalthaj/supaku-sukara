package com.scalabasics.exceptionhandling

import java.util.Calendar

object SampleExceptionHandling {

  def main(args: Array[String]) {
    val then = null
    val now = Calendar.getInstance()

    try {
      now compareTo then
    } catch {

      case e: NullPointerException =>
        println("null value"); System.exit(-1)
      case unknown: Any => println("Unknown exception " + unknown); System.exit(-1)
    } finally {
      println("worked")
    }

  }
}