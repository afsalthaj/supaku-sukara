package com.thaj.functionalprogramming.example.exercises

import com.thaj.functionalprogramming.example.exercises.PureStatefulAPI._

/**
  * Created by afsalthaj on 5/11/2016.
  * Please note that, the functions might look complex
  * The concept to be taken away from this sesion
  * is `state Actions` and it can behave in the same way
  * as any other types (or boxes) such as `Option`, `List`
  * etc. Hence sequence, traverse, map, map2, flatMap are valid here for easy composition of state actions.
  * hence, for simplicity a state action RNG => (A, RNG) is then represented
  * as Rand[A]... To have programmatic resemblance with other types
  */
object PureStatefulAPIAdvanced {

  // All the functions were of the type (RNG) => (Sometype, RNG)
  // It is known as state action - a program that depends on some RNG, uses it to generate some A,
  // and also transitions the RNG to a new state that can be used by another action later
  type Rand[+A] = RNG => (A, RNG)


  // We can turn methods such as RNG's nextInt into values of type Rand
  val int: Rand[Int] = _.nextInt

  // We want to write combinators that let us combine Rand actions while avoiding explicitly passing along the RNG state

  def unit[A](a: A): Rand[A] = {
    //(rng: RNG) => (a, rng)
    rng => (a, rng)
  }

  //There’s also map for transforming the output of a state action without modifying the state itself. Remember,
  // Rand[A] is just a type alias for a function type RNG => (A, RNG),
  // so this is just a kind of function composition:
  // You can see a state action similar to any othe boxes that we defined.. List, Option, Either, Validation etc.
  // In this case given a state action, it converts into another state action
  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = {
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2 )
    }
  }

  def nonNegativeInt: Rand[Int] = map(int)(t => if (t < 0) -t-1 else t)
  def double:Rand[Double] = map(nonNegativeInt)(a => a/Int.MaxValue.toDouble+1)
  def nonNegativeEven: Rand[Int] =  map(nonNegativeInt)(i => i - i % 2)

  /**
    * Unfortunately, map is not good enough to implement intDouble and doubleInt
    * What we need is a new combinator map2 that can combine two RNG actions into
    * one using a binary rather than unary function.
    * {{{
    *
    * def intDouble(rng: RNG): ((Int,Double), RNG) = {
        val (i1, r1) = nonNegativeInt(rng)
        val (d, r2) = double(rng)
        ((i1, d), r2)
      }
     // you could avoid case match here, you may work it out yourself
     def doubleInt(rng: RNG): ((Double,Int), RNG) = intDouble(rng) match {
       case ((a, b), c) =>  ((b,a), c)
     }
    *
    * }}}
    */

  def map2[A, B, C](a: Rand[A], b: Rand[B]) (f: (A, B)=> C): Rand[C] = rng => {
    val (i1, r1) = a(rng)
    val (i2, r2) = b(r1)
    (f(i1, i2), r2)
  }

  //if the above action f is generatiing a pair, then we can define a new function
  def both[A, B] (a: Rand[A], b: Rand[B]) =
    // map2(a, b)((a,b) => (a,b))
    map2(a, b)((_,_))

  //reimplementing double
  def doubleInt: Rand[(Double, Int)] = both(double, int)
  def intDouble: Rand[(Int, Double)] = both(int, double)

  //Exercise 6.7
  // Hard: If you can combine two RNG transitions, you should be able to combine a whole list of them.
  // Implement sequence for combining a List of transitions into a single transition.
  // Use it to reimplement the ints function you wrote before.
  // For the latter, you can use the standard library function List.fill(n)(x) to make a list with x repeated n times.
  // you may find this
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldLeft(unit(List[A]()))((a, b) => map2(b, a)(_ :: _))

  //val int: Rand[Int] = _.nextInt
  def ints(n: Int): Rand[List[Int]]= sequence(List.fill(n)(int))

  // send non-negative less than, which generates an integer between zero (inclusive) and n (exclusive)
  // However the generated value will be skewed for obvious reasons.
  // This will certainly generate a number in the range, but it’ll be skewed because Int.MaxValue may not be exactly
  // divisible by n. So numbers that are less than the remainder of that division will come up more frequently
  // Assume that, given n is 100, Int.MaxValue % 100 = 47; and most of the random generated will be less than 47
  // and hence skewed. Please run the spec to get an idea. Why this happens?
  // The maximum number that is divisible by 100 (that fit is 32 bit Integer) is Int.MaxValue - 47
  // Hence if n is between 0 and 47 or if n is between (Int.MaxValue - 47) and Int.MaxValue, the random number that you
  // generate will be less than 47. Hence the solution here is to limit the nonNegativeInt not to produce any number
  // that is greater than the maximum multiple of n. This is done in `nonNegativeLessThan`
  def nonNegativeLessThanSkewed(n: Int): Rand[Int] = map(nonNegativeInt)(_ % n)

  // A bit complicated, but you are on the right track as far as you can understand the above comments.
  def nonNegativeLessThan(n: Int): Rand[Int] = { rng =>
    val (i, rng2) = nonNegativeInt(rng)
    val mod = i % n
    if (i + (n-1) - mod>= 0 )
      (mod, rng2)
    else nonNegativeLessThan(n)(rng)
  }

  //But it would be better to have a combinator that does this passing along for us.
  // Neither map nor map2 will cut it. We need a more powerful combinator, flatMap
  // Exercise 6.8.
  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
  rng => {
    val (a, r) = f(rng)
    g(a)(r)
  }

  def nonNegativeLessThanFlatMap(n: Int): Rand[Int] =
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if (i + (n - 1) >= mod) unit(mod)
      else nonNegativeLessThanFlatMap(n)
    }

  // Exercise 6.9
  // Reimplement map and map2 in terms of flatMap. The fact that this is
  // possible is what we’re referring to when we say that flatMap is more
  // powerful than map and map2.
  def mapViaFlatMap[A,B](s: Rand[A])(f: A => B): Rand[B] =
  flatMap(s)(a => unit(f(a)))

  // Neednt be so much focussing here
  def map2ViaFlatMap[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
  // flatMap(ra)(a => flatMap(rb)(b => unit(f(a, b))))
    flatMap(ra)(a => map(rb)(b => f(a, b)))

}