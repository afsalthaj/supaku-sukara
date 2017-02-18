package com.thaj.functionalprogramming.example.exercises.part2

import com.thaj.functionalprogramming.example.exercises.PureStatefulAPI.RNG
import com.thaj.functionalprogramming.example.exercises.PureStatefulAPIGeneric.State


/**
 *  Although a library for testing has a very different purpose than a library for parallel computations,
 *  we’ll discover that these libraries
 *  have a lot of surprisingly similar combinators. This similarity is something we’ll return to in part 3.
 */
object PropertyBasedTestingSample {
  /*
    {{{
       // A generator of list of integers between 0 and 100
        val intList: Gen[List[Int]] = Gen.listOf(Gen.choose(0, 100))
        val prop = forAll(intList)(ns => ns.reverse.reverse == ns) &&
         forAll(intList)(ns => ns.headOption == ns.reverse.lastOption)
        val failingProp = forAll(intList)(ns = ns.reverse == ns
      )

      > prop.check
      > failingProp.check
    }}}

    The function forAll creates a property by combining a generator of type Gen[A] with some
    predicate of type A => Boolean. The property asserts that all values produced by the generator
    should satisfy the predicate. Like generators, properties can also have a rich API.
    In this simple example we’ve used && to combine two properties.
    The resulting property will hold only if neither property can be falsified by any of the generated test cases.
    Together, the two properties form a partial specification of the correct behavior of the reverse method.[1]



    Exercise 8.1
    To get used to thinking about testing in this way, come up with properties that specify the implementation
    of a sum: List[Int] => Int function. You don’t have to write your properties down as
    executable ScalaCheck code—an informal description is fine. Here are some ideas to get you started:

    -> List of all zeroes => zero
    -> reverse the list and sum => sum of list
    -> list of same number => number * size of list
    -> list of all 1s => size of list
    -> list of n numbers and negative of all numbers => zero


    Exercise 8.2
    What properties specify a function that finds the maximum of a List[Int]?
    max of list of zeros => zero
    max of reverse of a list => max of list
    max of list with Int.MaxValue => Int.MaxValue
   */

  /** A few nice properties of property based checking
    * API Generators and Properties
    *
    * Test Case minimisation: if a property fails, it tries to find out the smallest list of values, for which
    * the property was failing. This illuminates debugging.
    *
    * Exhaustive test case generation: If your domain Gen[A] is well defined, it runs test for all the values
    * in the domain and test, instead of taking a sample of values and run the test that can result in proving
    * a theorem by the absence of an evidence to the contrary.
    *
    * ScalaCheck is one property-based testing library
    * val intList = Gen.listOf(Gen.choose(0,100))
    * val prop = forAll(intList)(ns => ns.reverse.reverse == ns) &&
    * forAll(intList )(ns => ns.headOption == ns.reverse.lastOption)
    */

  // Initial API set up

  def listOf[A](gen:Gen[A]): Gen[List[A]] = ???

  // bringing in size constrains

  /**
   * This would certainly be a useful combinator, but not having to explicitly specify sizes is powerful as well.
   * It means that whatever function runs the tests has the freedom to choose test case sizes, which opens up the
   * possibility of doing the test case minimization we mentioned earlier. If the sizes are always fixed and specified by
   * the programmer, the test runner won’t
   * have this flexibility. Keep this concern in mind as we get further along in our design.
   */
  def listOfN[A](n: Int, gen: Gen[A]): Gen[List[A]] = ???

  def forAll[A](a: Gen[A])(f: A => Boolean): Prop = ???

}


trait LegacyProp {
  def check: Boolean
  /**
   * Exercise 8.3
   */
  def &&(p: LegacyProp): LegacyProp = new LegacyProp {
    def check = this.check && p.check
  }
}


// Design refinement
trait Prop {
  import Prop._
  def check: Either[(FailedCase, SuccessCount), SuccessCount]
}
object Prop {
  type FailedCase = String
  type SuccessCount = Int
}

// Consider gen as something dealing with random number generator, that in turn deals with state transitions.
// Lets wrap State transition with respect to RNG in Gen case class

case class Gen[A](sample: State[RNG, A])



object Gen {
  //Exercise 8.4
  //Implement Gen.choose using this representation of Gen.
  // It should generate integers in the range start to stopExclusive. Feel free to use functions you’ve already written.

  import State._

  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(nonNegativeInt.map(n => start + n % (stopExclusive-start)))

  // Exercise 8.5
  def unit[A](a: A): Gen[A] = Gen(State.unit(a))
  def boolean: Gen[Boolean] = Gen(State.boolean)
  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = Gen(State.sequence(List.fill(n)(g.sample)))
}