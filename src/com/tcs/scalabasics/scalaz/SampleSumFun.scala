package com.tcs.scalabasics.scalaz

object Sum {

  def main(args: Array[String]) {
    println(sum(List(1, 2, 3)))
  }

  def sum(x: List[Int]) = x.foldLeft(0) { _ + _ }

}

