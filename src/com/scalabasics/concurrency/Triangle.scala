package com.scalabasics.concurrency
/**
 * Triangle is a class that extends the abstract class Shape
 */
class Triangle(val point1: Point, val point2: Point, val point3: Point) extends Shape {

  def draw() = println(this)

  override def toString() = "Triangle is " + point1 + "," + point2 + "," + point3
}


  