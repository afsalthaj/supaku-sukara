package main.scala.com.thaj.functionalprogramming.example.exercises

import scala.annotation.tailrec

/**
 * Created by afsalthaj on 10/10/16.
 */
object Recursion {
  def factorial (n: Int): Int = {
    @tailrec
    def go (n: Int, acc: Int): Int =
      if (n <= 0) acc
    else
        go (n-1, acc * n)
    go(n, 1)
  }

  // Write a recursive function to get the
  // nth Fibonacci number (http://mng.bz/C29s).
  // The first two Fibonacci numbers are 0 and 1.
  // The nth number is always the sum of the previous two—the
  // sequence begins 0, 1, 1, 2, 3, 5. Your definition should use a local tail-recursive function.

  def getNthFibonacci (n: Int) = {
    @tailrec
    def loop (inc: Int, prev: Int, current: Int): Int =
      if (inc >= n) current
      else
        loop(inc + 1, current,  prev + current)

    if (n == 0) 0 else loop (0, 0, 1)
  }
}
