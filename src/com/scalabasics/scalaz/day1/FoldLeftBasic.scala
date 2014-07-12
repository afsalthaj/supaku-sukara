package com.scalabasics.scalaz.day1

object FoldLeftBasic {

  def foldLeftBasic[A, B](xs: List[B], a: A, f: (A, B) => A): A = xs.foldLeft(a)(f)

}

trait SampleBasicMonoid[A] {

  def foldLeftFunction(a: A, b: A): A
  def foldLeftInitialValue: A
}

object SampleMonoidIntImpl extends SampleBasicMonoid[Int] {

  def foldLeftFunction(a: Int, b: Int) = a + b
  def foldLeftInitialValue: Int = 0

}

object RealFunctionsToBeCalled {
  implicit val sampleBasicMonoidImpl = SampleMonoidIntImpl
  def sum[A](xs: List[A])(implicit m: SampleBasicMonoid[A]): A = {

    FoldLeftBasic.foldLeftBasic(xs, m.foldLeftInitialValue, m.foldLeftFunction)
  }
}

object CallFunctions {
  def main(args: Array[String]) {
    println(RealFunctionsToBeCalled.sum(List(1,2))(RealFunctionsToBeCalled.sampleBasicMonoidImpl))
  }
}