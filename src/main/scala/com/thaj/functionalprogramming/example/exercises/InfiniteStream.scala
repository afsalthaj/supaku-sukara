package com.thaj.functionalprogramming.example.exercises

/**
  * Created by afsalthaj on 30/10/2016.
  */
object InfiniteStream {

  // Because they’re incremental, the functions we’ve written also work for
  // infinite streams. Here’s an example of an infinite Stream of 1s:
  val ones: Stream[Int] = Stream.cons(1, ones)
  // Although ones is infinite, the functions we’ve written so
  // far only inspect the portion of the stream needed to generate the demanded output. For example

  val x = ones.take(5).toList
  // But sometime it can lead to stack overflow errors as well
  // ones.forAll(_ == 1)

  // Exercise 5.8
  //Generalize ones slightly to the function constant, which returns an infinite Stream of a given value.

  def constant[A](a: A): Stream[A] = Stream.cons(a, constant(a))
  // TODO: 5.8 to 5.16 exercises; Lets move forward and connect the dots afterwards. Y
  // You know the concept of Stream, and how it is implemented, importance of thunks, and the incremental
  // approach of stream in computations, efficient garbage collection, efficient memory utilization etc

  /**
    * 5.5 Summary
    * In this chapter, we introduced non-strictness as a
    * fundamental technique for implementing efficient and modular
    * functional programs. Non-strictness can be thought of as a technique for recovering
    * some efficiency when writing functional code, but it’s also a much bigger idea—non-strictness can
    * improve modularity by separating the description of an expression from the how-and-when of its evaluation.
    * Keeping these concerns separate lets us reuse a description in multiple contexts, evaluating different portions
    * of our expression to obtain different results. We weren’t able to do that when description and evaluation were
    * intertwined as they are in strict code. We saw a number of examples of this principle in action over the course
    * of the chapter, and we’ll see many more in the remainder of the book. We’ll switch gears in the next chapter
    * and talk about purely functional approaches to state. This is the last building block needed before we begin
    * exploring the process of functional design.
    */
}
