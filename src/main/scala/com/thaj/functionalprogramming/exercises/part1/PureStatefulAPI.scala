package com.thaj.functionalprogramming.example.exercises

/**
  * Created by afsalthaj on 30/10/2016.
  */
object PureStatefulAPI {

  /**
    * This problem of making seemingly stateful APIs pure and its solution (having the API
    * compute the next state rather than actually mutate anything) aren’t unique to random number generation.
    * It comes up frequently, and we can always deal with it in this same way
    */

   // An example of a pure API. Please note that the new instance of RNG is a
   // return type. You have to use the returned rng to produce the next random value
   trait RNG {
     def nextInt: (Int, RNG)
   }

   case class SimpleRng(seed: Long) extends RNG {
     def nextInt: (Int, RNG) = {
       //& is a bit wise AND. We use current seed to generate a new seed
       val intermediateResult = seed * 0x5DEECE66DL + 0xBL
       val newSeed = intermediateResult & 0xFFFFFFFFFFFFFL
       val newRng = SimpleRng(newSeed)
       val intermediateResult2 = newSeed >>> 16
       val int = intermediateResult2.toInt
       (int, newRng)
    }
   }

   def getRandomNumbersTwice(rng: RNG): ((Int, Int), RNG) = {
     val (i1, rng1) = rng.nextInt
     val (i2, rng2) = rng1.nextInt
     ((i1, i2), rng2)
   }

   // Exercise 6.2
   // Write a function that uses RNG.nextInt to generate a random integer between 0 and Int.maxValue (inclusive).
   // Make sure to handle the corner case when nextInt returns Int.MinValue,
   // which doesn’t have a non-negative counterpart.
   def nonNegativeInt(rng: RNG): (Int, RNG) = rng.nextInt match {
     case (i, r) if i < 0 => (-i-1, r)
     case t => t
   }

  // Exercise 6.3
  // Write a function to generate a Double between 0 and 1, not including 1. Note: You can use Int.MaxValue
  // to obtain the maximum positive integer value, and you can use x.toDouble to convert an x: Int to a Double.

  // you could avoid case match here, you may work it out yourself
  def double(rng: RNG): (Double, RNG) = nonNegativeInt(rng) match {
    case (i, r) => (i/(Int.MaxValue.toDouble+1), r)
  }

  // Exercise 6.39u
  // Write functions to generate an (Int, Double) pair, a (Double, Int) pair,
  // and a (Double, Double, Double) 3-tuple. You should be able to reuse the functions you’ve already written.
  // adding a few constraints such as it should be nonnegative int, and hence double, and should be handling
  // same rng at the same time
  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i1, r1) = nonNegativeInt(rng)
    val (d, r2) = double(rng)
    if(r1 == r2)
     ((i1, d), r2)
    else
      // this is non functional approach of handling things, However this is shown just to teach you that
      // r1 and r2 are always the same for a given `rng`. This exception never occurs
      // Refer RngSpec with property based checking
      throw new Exception()
  }

  // you could avoid case match here, you may work it out yourself
   def doubleInt(rng: RNG): ((Double,Int), RNG) = intDouble(rng) match {
     case ((a, b), c) =>  ((b,a), c)
   }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)
  }

  // Exercise 6.4
  // Write a function to generate a list of random integers.
  def ints(count: Int)(rng: RNG):(List[Int], RNG) = {
    def innerInts(values: List[Int], count: Int, newR: RNG): (List[Int], RNG) = {
      if (count == 0)
        (Nil, rng)
      else {
        val (value, r) = rng.nextInt
        innerInts( value :: values, count - 1, r)
      }
    }

    innerInts(Nil, count, rng )
  }
}
