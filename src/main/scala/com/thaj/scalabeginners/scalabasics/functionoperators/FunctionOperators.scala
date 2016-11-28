package com.scalabasics.functionoperators

/**
 * examples of function operators
 */
object FunctionOperators {

  def main(args: Array[String]) {
    //doing the same thing in different ways in scala
    val list = List(2, 3, 4)
    //1 is added temporarily to the left side of list but ; means the below referred list will have only 2,3 and 4
    1 :: list foreach ((n: Int) => print(n))
    println
    list.foreach(print(_))
    println
    list.foreach(print)
    println
    print("First method counts the number of values that are divisible by 2 " + list.count((n: Int) => n % 2 == 0))
    println
    println("below methods list out the values that are divisible by 2")
    list filter ((n: Int) => getTrueFalse(n)) foreach (print(_))
    println
    list filter getTrueFalse foreach print
    println
    list filter getTrueFalseTwo foreach print
    println
    list filter ((n: Int) => n % 2 == 0) foreach print
    println

    list.++(List(5, 6)).foreach(print(_))

  }

  def getTrueFalse(n: Int) = {

    if (n % 2 == 0)
      true
    else {
      false
    }
  }

  def getTrueFalseTwo(n: Int) = (n % 2) == 0
}