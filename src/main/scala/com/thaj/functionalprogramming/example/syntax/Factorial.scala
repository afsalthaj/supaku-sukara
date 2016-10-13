package main.scala.com.thaj.functionalprogramming.example.syntax

import scala.annotation.tailrec

object MyModule {
  def factorial (a: Long): Long = {
    @tailrec
    def go(n: Long, acc: Long): Long =
    if (n <= 0) acc
    else
      go(n-1, acc * n)

    go(a, 1)
  }
}