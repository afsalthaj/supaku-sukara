package com.thaj.functionalprogramming.example.exercises.part2

import com.thaj.functionalprogramming.example.exercises.PureStatefulAPI.{SimpleRng, RNG}
import com.thaj.functionalprogramming.example.exercises.PureStatefulAPIGeneric.State
import scalaz._
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
trait PropRefinedV1 {
  import PropRefinedV1._
  def check: Either[(FailedCase, SuccessCount), SuccessCount]
}
object PropRefinedV1 {
  type FailedCase = String
  type SuccessCount = Int
}


// Consider gen as something dealing with random number generator, that in turn deals with state transitions.
// Lets wrap State transition with respect to RNG in Gen case class

// Gen[RNG, Gen[A](State (rng => (genA, rng2))) : Gen[Gen[A]]
case class Gen[A](sample: State[RNG, A]) {
  def map [B](a: A => B): Gen[B] = Gen(this.sample.map(a))
  // Exercise 8.6
  def flatMap[B](a: A => Gen[B]): Gen[B] = Gen.join(this.map(a))
  def listOfN(size: Int): Gen[List[A]] = listOfN(Gen.unit(size))
  def listOfN(size: Gen[Int]): Gen[List[A]] = size.flatMap(n => Gen.sequence(List.fill(n)(this)))
  def map2[B, C](a: Gen[B])(f: (A, B) => C): Gen[C] = this.flatMap( t => a.map(aa => f(t, aa)))
  // Exercise 8.10
  def unsized: SGen[A] = SGen(_ => this)

}



object Gen {
  //Exercise 8.4
  //Implement Gen.choose using this representation of Gen.
  // It should generate integers in the range start to stopExclusive. Feel free to use functions you’ve already written.

  import State._

  // defines a generator that can generate numbers between start and stopExclusive
  // Please note that it could generate infinite numbers that range between start and stopExclusive
  // From this we can somehow guess, we are moving towards
  // using `Stream` in this API
  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(nonNegativeInt.map(n => start + n % (stopExclusive-start)))

  // Exercise 8.5
  // It can be considered as a continuation, where you got a number and an associated state which doesn't really matter
  def unit[A](a: A): Gen[A] = Gen(State.unit(a))
  def boolean: Gen[Boolean] = Gen(State.boolean)
  // generates list of length n using generator g. This is pretty interesting concept going forward. Please note
  // that it could generate infinite number of lists of length n. From this we can somehow guess, we are moving towards
  // using `Stream` in this API
  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = Gen(State.sequence(List.fill(n)(g.sample)))

  /**
   * As we discussed in chapter 7, we’re interested in understanding what operations are
   * primitive and what operations are derived, and in finding a small yet expressive set of primitives.
   * A good way to explore what is expressible with a given set of primitives is to pick some
   * concrete examples you’d like to express, and see if you can assemble the functionality you want.
   * As you do so, look for patterns, try factoring out these patterns into combinators, and refine your set of primitives.
   * We encourage you to stop reading here and simply play with the primitives and combinators we’ve written so far.
   * If you want some concrete examples to inspire you, here are some ideas:
   * If we can generate a single Int in some range, do we need a new primitive to generate an (Int,Int) pair in some range?
   * Can we produce a Gen[Option[A]] from a Gen[A]? What about a Gen[A] from a Gen[Option[A]]?
   * Can we generate strings somehow using our existing primitives?
   *
   * // Refer to map, flatMap, join etc
   */

  def join[B](a: Gen[Gen[B]]): Gen[B] = {
    Gen(State(rng => {
      val k: (Gen[B], RNG) = a.sample.run(rng)
      val y: (B, RNG) = k._1.sample.run(k._2)
      y
    }))
  }

  def sequence[A](a: List[Gen[A]] ): Gen[List[A]] =
    a.foldRight(unit(Nil: List[A]))((c, d) => c.map2(d)(_ :: _))


  // Sample ones
  def converToGenOptionA[A](a: Gen[A]): Gen[Option[A]] = a.map(Option(_))

  //Tuple in some range
  def GenForTuple(start: Int, end: Int): Gen[(Int, Int)] = Gen.choose(start, end).map2(Gen.choose(start, end))((_, _))

  //generate string - May be wrong, but just for pedagogical purpose
  def generateString = Gen.choose(1, 100).map(_.toString)

  // Suppose we’d like a Gen[(String,String)] that generates pairs where the second string
  // contains only characters from the first. Or that we had a Gen[Int] that chooses an integer
  // between 0 and 11, and we’d like to make a Gen[List[Double]] that then generates
  // lists of whatever length is chosen. In both of these cases there’s a dependency—we

  def getIntegerBetween0and11(n: Int): Gen[List[Double]] = sequence(List.fill(n)(Gen.choose(0, 11).map(_.toDouble)))

  // This might be wrong
  def getStringPairWithSecondStringBeingSubStringOfFirst: Gen[(String, String)] = {
    Gen.choose(0, 100).map(_.toString).map2(Gen.choose(0, 100).map(_.toString)) { (a, b) =>
      if(a.contains(b))(a, b) else (a, a)
    }
  }

  // Exercise 8.7
  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    boolean.flatMap(b => if (b) g1 else g2)

  // Exercise 8.8
  // Implement weighted, a version of union that accepts a weight for each
  // Gen and generates values from each Gen with probability proportional to its weight.
  // Referred the answer here
  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A],Double)): Gen[A] = {
    val g1Threshold = g1._2.abs / (g1._2.abs + g2._2.abs)

    Gen(State.double).flatMap(d => if (d < g1Threshold) g1._1 else g2._1)
  }
}


import Prop._


case class Prop(run: (MaxSize, TestCases, RNG) => Result) {

  // Exercise 8.9
  // defect with this implementation it runs almost together, but that's fine. Not sure if only Prop has to be
  // executed in case of failure.
  // this implementation as of now seems to be straight forward (than the one in fpinscala),
  // and doesn't have the usual the boolean && behaviour
  def && (x: Prop): Prop = Prop({ (max, n, rng) => {
    val result = this.run(max, n,rng)
    val result2 = x.run(max, n,rng)
    (result, result2) match {
      case (Falsified(a, b), Falsified(c, d)) => Falsified(a.toString + c.toString, b + d)
      case (Passed, Falsified(a, b)) => Falsified(a.toString, n + b )
      case (Falsified(a, b), Passed) => Falsified(a.toString, n + b)
      case _ =>  Passed
    }
  }})

  // I personally didn't like the fact anding a property result in sequential execution
  // Hence I added a sequentialAnd to combine multiple properties such that,
  // if the first property fails, the whole test case fails
  def sequentialAnd (x: Prop): Prop = Prop({ (max, n, rng) => {
    this.run(max, n,rng) match {
      case Passed => x.run(max,n, rng) match {
        case Falsified(a, b) => Falsified(a.toString, n + b)
        case a => a
      }
      case a => a
    }
  }})

  // differs from the other solutions, but its simpler and readable and does the same thing.
  def || (x: Prop): Prop = Prop ( { (max, n, rng) => {
    val result1 = this.run(max, n, rng)
    result1 match {
      case Falsified(a, b) => x.run(max, n, rng) match {
        case Passed => Passed
        case Falsified(c, d ) => Falsified(a.toString + c.toString, b + d)
      }
      case _ => Passed
    }
  }})
}


// Prop Refined - Final Version
object Prop {
  type MaxSize = Int
  type FailedCase = String
  type SuccessCount = Int
  type TestCases = Int

  /**
   *
   * type Result = Either[(FailedCase, SuccessCount), SuccessCount]
   * case class Prop(run: TestCases => Result)
   * Also, we’re recording the number of successful tests on both sides of that Either.
   * But when a property passes, it’s implied that the number of passed tests will be equal to the argument to run.
   * So the caller of run learns nothing new by being told the success count.
   * Since we don’t currently need any information in the Right case of that Either, we can turn it into an Option:
   * type Result = Option[(FailedCase, SuccessCount)]
   * case class Prop(run: TestCases => Result)
   * This seems a little weird, since None will mean that all tests succeeded
   * and the property passed and Some will indicate a failure. Until now, we’ve only
   * used the None case of Option to indicate failure. But in this case we’re using it
   * to represent the absence of a failure.
   * That’s a perfectly legitimate use for Option, but its intent isn’t very clear.
   * So let’s make a new data type, equivalent to Option[(FailedCase, SuccessCount)], that shows our intent very clearly.
   */
  sealed trait Result {
    def isFalsified: Boolean
  }

  case object Passed extends Result {
    def isFalsified: Boolean = false
  }

  case class Falsified(failedCase: FailedCase, successCount: SuccessCount) extends Result {
    def isFalsified: Boolean = true
  }

  import com.thaj.functionalprogramming.example.exercises.Stream
  // Basically generates a stream of A by intuitively passing the changed state after each computation of stream.
  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] = {
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))
  }

  // define forAll now, which takes a Gen[A] and a predicate..and it returns a Prop, and to `run` prop
  // we need n (TestCases) and RNG
  // So n here is the number of test cases basically.
  // Ex: if A is `Int` then it generates n integer values and apply it to the
  // predicate (which is possibly the function to be tested). Hence we are testing
  // the function `n` times. Hence n refers to `TestCases`
  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (max,n,rng) => randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
      case (a, i) => try {
        if (f(a)) Passed else Falsified(a.toString, i)
      } catch { case e: Exception => Falsified(buildMsg(a, e), i) }
    }.find(_.isFalsified).getOrElse(Passed)
  }


  def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
    forAll(g.forSize)(f)

  // this is basically this much
  // You know have to run `max` number of test cases.
  // You provide an n, and it generates a Gen for that.
  // A gen will give you one property with the help of `forAll`
  // Now you pass the next `n` and get the next `Gen` and get another property.
  // So now you know why there is a function Int => Gen[A] in the below function. Also,
  // please note that the next Gen will generate more test cases than previous
  // one, and once you reach the max, you && all the properties. and then you call run
  // you are done!
  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop( {
    (max, n, rng) => {

      val casesPerSize = ( n + (max - 1)) / max

      // based on an int, you will have a Gen
      // assume n = 10, max = 10, the only thing thats going to happen here
      // It makes sure that the minimum between n and max is selected to ensure that
      // we are not stuffing up starting from the maximum values. We add plus 1 to ensure
      // we have n number of properties as we are starting from zero. So right now, we have
      // a stream starting from 0 to the minimum between n and max + 1, and we have that many number
      // of generators. This implies, we have that many number of properties. Each of those properties
      // expect a number of test cases to run, maximum number of test cases and rng state. Hence we have to combine
      // all these properties using && and provide these numbers only once.
      val props: Stream[Prop] = Stream.from(0).take(n min max + 1).map( i => forAll(g(i))(f))
      // Now if we pass a single max and n to all properties, then we have equal number of test cases
      val prop: Prop = props.map(eachProp => {
        Prop ( { case (m, _, r) => eachProp.run(m, casesPerSize, r)
        })
      }).toList.reduce(_ sequentialAnd _)

      prop.run(max, n, rng)
    }
  })

  // building an error message - needn't get your head around this.
  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

  // A helper function to run the properties with default arguments
  def run(p: Prop,
          maxSize: Int = 100,
          testCases: Int = 100,
          rng: RNG = SimpleRng(System.currentTimeMillis)): Unit =
    p.run(maxSize, testCases, rng) match {
      case Falsified(msg, n) =>
        println(s"! Falsified after $n passed tests:\n $msg")
      case Passed =>
        println(s"+ OK, passed $testCases tests.")
    }
}


// Test Case Minimisation
// Instead of starting from a wide set of test cases and then `shrinking` it down to find the
// test case that failed, we slowly generate test case and then widens up..(sized generation)
// Instead of modifying our Gen data type, for which we’ve already written a number of useful combinators,
// let’s introduce sized generation as a separate layer in our library. A simple representation of a sized generator is just a function that takes a size and produces a generator:
// Exercise 8.11
case class SGen[A](forSize: Int => Gen[A]) {
  def map [B](a: A => B): SGen[B] = SGen(n => this.forSize(n).map(a))
  // Exercise 8.6
  def flatMap[B](a: A => SGen[B]): SGen[B] = SGen.join(this.map(a))
  def listOfN(size: SGen[Int]): SGen[List[A]] = size.flatMap(n => SGen.sequence(List.fill(n)(this)))
  def map2[B, C](a: SGen[B])(f: (A, B) => C): SGen[C] = this.flatMap( t => a.map(aa => f(t, aa)))
}

object SGen {
  def unit[A](a: A): SGen[A] = SGen(n => Gen.unit(a))
  def join[A](sgen: SGen[SGen[A]]): SGen[A] = SGen ({ n => {
      val gen = sgen.forSize(n)
      val genOfGen: Gen[Gen[A]] = gen.map(a => a.forSize(n))
      Gen.join(genOfGen)
    }})

  def sequence[A](a: List[SGen[A]] ): SGen[List[A]] =
    a.foldRight(unit(Nil: List[A]))((c, d) => c.map2(d)(_ :: _))

  def listOf[A](g: Gen[A]): SGen[List[A]] = SGen(n => g.listOfN(n))

  // An example that results in a failure
  def smallInt = Gen.choose(-10,10)
  def maxProp = forAll(listOf(smallInt)) { ns =>
    val max = ns.max
    !ns.exists(_ > max)
  }

  // Exercise 8.13
  // Define listOf1 for generating nonempty lists, and
  // then update your specification of
  // max to use this generator.
  def listOfl[A](g: Gen[A]): SGen[List[A]] = SGen(n => g.listOfN(n max 1))

  // Write a property to verify the behavior of List.sorted
  // For instance,
  /// List(2,1,3).sorted is equal to List(1,2,3).

  // EXERCISE 8.14
  // Something like this
  def genInt = Gen.choose(0, 100)
  def genListOfInt = listOf(genInt)
  def propertyTestingSorting = forAll(genListOfInt) { list => {
    val ls = list.sorted
    list.isEmpty ||  list.tail.isEmpty || !(ls.head > ls.tail.reverse.head)
  }}
}