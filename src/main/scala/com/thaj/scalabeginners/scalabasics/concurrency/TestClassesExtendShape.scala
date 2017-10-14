package com.scalabasics.concurrency

object testThisAbstractClass {

  def main(args: Array[String]) {

    val x: Triangle = new Triangle(new Point(1, 2), new Point(3, 4), new Point(4, 5))

    x.draw

    val y: Rectangle = new Rectangle(new Point(1, 2), 3, 4)

    y.draw

  }
}