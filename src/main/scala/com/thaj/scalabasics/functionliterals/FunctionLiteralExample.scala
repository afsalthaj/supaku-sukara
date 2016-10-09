package com.scalabasics.functionliterals

/**
 * examples of function literals
 * understand each function and the way scala can manipulate variables
 */
class FunctionLiteralExample {
  /**
   * function that accepts an array of String and then converts each of them into upper case and
   * returns converted array of String
   */
  def changeCase(x: Array[String]): Array[String] = {

    return x.map((s: String) => s.toUpperCase())

  }
  /**
   * function that accepts any number of string and then map to upper case
   */
  def changeCase(args: String*) = {
    args.map((eachString: String) => eachString.toUpperCase())
  }

}

object MainClass {

  def main(args: Array[String]) {
    val classInstance = new FunctionLiteralExample
    val y: Array[String] = classInstance.changeCase(Array("NAME1", "NAME2", "NAME3", "NAME4"))
    printy(y)
    println
    printyAsFunctionDef(y)
    println
    println(classInstance.changeCase("NAME1", "NAME2", "NAME3", "NAME4"))
    println()
    transformAndPrintY(y)

  }
  //anonymous function
  val printy = (x: Array[String]) => x.foreach(println(_))

  //example of a function defintion

  def printyAsFunctionDef(x: Array[String]): Unit = {

    x.foreach(printf("%s ", _))

  }

  //anonymous function that takes an array of String and transforms it

  val transformAndPrintY = (x: Array[String]) => x.foreach((eachString: String) => println(tagEachOnewithLov(eachString)))

  def tagEachOnewithLov(x: String) = {

    x match {
      case "NAME1" => (x + "-LOV1")
      case "NAME2" => (x + "-LOV2")
      case "NAME3" => (x + "-LOV3")
      case "NAME4" => (x + "-LOV4")
    }
  }

}