package com.scalabasics.patternmatching
import scala.util.matching.Regex
import scala.util.control.Breaks

object SampleRegex {

  //different types of date patterns (.r function will convert the string to regex pattern (Regex))
  val datePatterns = List(
    "(20[0-9]{2})[/-:]([0-9]{2})[/-:]([0-9]{2}).*".r,
    "([0-9]{2})[/-:]([0-9]{2})[/-:](20[0-9]{2}).*".r
  )

  var countMatchStrings = 0
  //initializing Breaks so that break can be applied in for loop
  val loop = new Breaks;

  def main(args: Array[String]) {

    println(isDatePattern("22/09/2009", "2013/09/09", "2013:09:08:11:11:11"))
    println(isDatePattern("ddd", "2012/09/09"))

  }

  //if all the strings match with a date pattern the function returns true
  def isDatePattern(x: String*): Boolean = {

    countMatchStrings = 0
    x foreach ((y: String) => {
      loop.breakable {
        for (datePattern <- datePatterns) {
          if ((datePattern findFirstIn y).nonEmpty) { countMatchStrings += 1; loop.break }
        }

      }
    })

    countMatchStrings == x.size
  }

}