package com.thaj.functionalprogramming.example.exercises

import scala.annotation.tailrec

/**
 * Created by afsalthaj on 10/10/16.
 */
class HOFExamples {
  def findFirstIn(a: Array[Int], int: Int) = {
    @tailrec
    def go(acc: Int): Int =
      if (acc >= a.length) -1
      else if (a(acc) == int) acc
      else go(acc + 1)
    go(0)
  }

  // generic function => parametric polymorphism
  def findFirstIn[A](a: Array[A], f: A => Boolean) = {
    val function = (a(_)) andThen f
    @tailrec
    def go(acc: Int): Int =
      if (acc >= a.length) -1
      else if (function(acc)) acc
      else go(acc + 1)
    go(0)
  }
}