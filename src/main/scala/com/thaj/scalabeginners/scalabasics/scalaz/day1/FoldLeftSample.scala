package com.scalabasics.scalaz.day1

object FoldLeftSample {

  def foldLeft[A, B](xs: List[B], a: A, f: (A, B) => A) = xs.foldLeft(a)(f)

}

// trait of function definitions for a particular type

trait CertainFunctionsMonoid[A, B] {
  def commonFoldLeftFunc(a: A, b: B): A
  def commonFoldLeftDefValue: A

}
// object that implements the functions in the above trait.. these functions can be passed
object CertainFunctionsMonoidImpl extends CertainFunctionsMonoid[List[String], String] {
  def commonFoldLeftFunc(a: List[String], b: String): List[String] = b :: a
  def commonFoldLeftDefValue: List[String] = List()
}

// object that defines the function f in FolLeftSample object

object RealFunctionsCalledFromOutside {
  implicit val certainFunctionsMonoidImpl = CertainFunctionsMonoidImpl
  def reverseList[C](xs: List[C])(implicit m: CertainFunctionsMonoid[List[C], C]) = {
    FoldLeftSample.foldLeft(xs, m.commonFoldLeftDefValue, m.commonFoldLeftFunc)
  }
}

// main function
object MainFunction {
  def main(args: Array[String]) {
    println(RealFunctionsCalledFromOutside.reverseList(List("1", "2"))(RealFunctionsCalledFromOutside.certainFunctionsMonoidImpl))
  }
}

