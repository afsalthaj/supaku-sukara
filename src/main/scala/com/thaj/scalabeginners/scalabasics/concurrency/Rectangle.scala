package com.scalabasics.concurrency

/**
 * Rectangle is a class that extends the abstract class Shape
 */
class Rectangle(val anyOnePoint: Point, val height: Double, val width: Double) extends Shape {

  def draw() = println(this)

  override def toString() = "Rectangle is " + anyOnePoint + "," + height + "," + width

}