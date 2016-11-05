package com.thaj.functionalprogramming.example.exercises

import com.thaj.functionalprogramming.example.exercises.PureStatefulAPI._

/**
  * Created by afsalthaj on 5/11/2016.
  */
class PureStatefulAPIAdvanced {

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
  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = {
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2 )
    }
  }

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

}
