package com.thaj.functionalprogramming.example.exercises

/**
 * Created by afsalthaj on 10/10/16.
 */
object HOF {

  // Exercise 2.3
  // Let’s look at another example, currying,[9] which converts a function f
  // of two arguments into a function of one argument that partially applies f.
  // Here again there’s only one implementation that compiles. Write this implementation.
  // This is named after the mathematician Haskell Curry, who discovered the principle.
  // It was independently discovered earlier by Moses Schoenfinkel, but Schoenfinkelization didn’t catch on.
  // We’ll often group and order our function arguments into multiple argument lists to maximize type inference.
 def curry[A,B,C](f: (A, B) => C): A => (B => C) =
    (a: A) => f(a, _)

 // Exercise 2.4
 // Implement uncurry, which reverses the transformation of curry.
 // Note that since => associates to the right, A => (B => C) can be written as A => B => C.
 def uncurry[A,B,C](f: A => B => C): (A, B) => C = (a: A, b: B) => f(a)(b)

  // Let’s look at a final example, function composition, which feeds the output of one function to the input of another function.
  // Again, the implementation of this function is fully determined by its type signature.

  def compose1[A,B,C](f: B => C, g: A => B): A => C = (a: A) => f(g(a))
  // or
  def compose2[A,B,C](f: B => C, g: A => B): A => C = f compose g

  // or
  def compose3[A, B, C](f: B => C, g: A => B): A => C = g andThen f

  //an example of andThen
  val f = (c: Double) => math.Pi/ 2 - c

  val cos : Double => Double = f andThen math.sin
}