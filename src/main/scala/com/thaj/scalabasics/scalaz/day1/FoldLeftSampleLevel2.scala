package com.scalabasics.scalaz.day1

// a trait that consist of foldLeft function that accepts any CollectionType[AnyType]
// and converts each of those type to A that will be initial value type in this definition
trait FoldLeftSampleLevel2[F[_]] {

  def foldLeft[A, B](xs: F[B], a: A, f: (A, B) => A): A

}

// consists of the definition that implements the trait function in a generic way for List type
object FoldLeftSampleLevel2Imple extends FoldLeftSampleLevel2[List] {

  def foldLeft[A, B](xs: List[B], a: A, f: (A, B) => A): A = xs.foldLeft(a)(f)

}

// trait for the initial value of foldLeftFunction (a) and the function that really works inside foldLeft (f)
trait SampleBasicMonoid1[A] {

  def foldLeftFunction(a: A, b: A): A = ???
  def foldLeftInitialValue: A = ???
}

// object that implements the above trait
object SampleMonoidIntImpl1 extends SampleBasicMonoid1[Int] {

  override def foldLeftFunction(a: Int, b: Int): Int = a + b
  override def foldLeftInitialValue: Int = 0

}

object RealFunctions {

  def someFunction[M[_]: FoldLeftSampleLevel2, A: SampleBasicMonoid1](xs: M[A]): A = {

    val m = implicitly[SampleBasicMonoid1[A]]
    val fl = implicitly[FoldLeftSampleLevel2[M]]
    fl.foldLeft(xs, m.foldLeftInitialValue, m.foldLeftFunction)

  }

  def main(args: Array[String]) {
    implicit val m1 = SampleMonoidIntImpl1
    implicit val z = FoldLeftSampleLevel2Imple
    someFunction(List(1, 2))(z, m1)
  }

}

  
  
/**In the above example, the traits SampleMonoidIntImpl1 and 
 FoldLeftSampleLevel2 correspond to Haskell’s typeclass. Scalaz provides many typeclasses. 
  */
