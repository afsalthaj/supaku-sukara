package com.scalabasics.patternmatching
/**
 * class comprises of basic function operators as well as Guarding
 */
object MatchingCaseClass {
  def main(args: Array[String]) {

    val list1 = List(1, 2, 4, 5, 6, 6, 7, 7, 8, 8)

    list1 filter ((n: Int) => n % 2 == 0) foreach ((n: Int) => print(n))
    println
    list1 filter ((n: Int) => n % 2 == 0) foreach (print(_))
    println
    list1 filter (_ % 2 == 0) foreach print
    println
    list1 filter getEven foreach print
    println
    list1 filter ((n: Int) => getEvenSecondType(n)) foreach print
    println

    val listOfEvenNumber = for { n <- list1 if (n % 2 == 0) } yield n

    for (eachEvenNumber <- listOfEvenNumber) {

      print(eachEvenNumber)
    }

    println
    val list2 = List(1, 2, 4, 5, 6, 6, 7, 7, 8, 8, "Afsal")

    list2 filter getEvenNumbers foreach print

    println
    list2.filter(getEvenNumberSecondType).foreach(print)

  }

  //simple case match
  def getEvenNumbers(n: Any): Boolean = {
    n match {
      case n: Int => n % 2 == 0
      case others => false
    }
  }

  // simple function literal
  def getEven(n: Int) = n % 2 == 0

  //simple if statement
  def getEvenSecondType(n: Int): Boolean = {

    if (n % 2 == 0) {
      true
    } else {
      false
    }
  }

  // pattern matching with GUARDING
  def getEvenNumberSecondType(n: Any) = n match {
    //case match with Guarding
    case n: Int if (n % 2 == 0) => true
    case n: Any                 => false
  }
}