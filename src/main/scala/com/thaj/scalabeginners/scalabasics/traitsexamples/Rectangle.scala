package com.scalabasics.traitsexamples

import com.scalabasics.concurrency.Point

class Rectangle(val x: Point, val y: Int, val z: Int) extends Shape with TraitSample {

  def draw() = println(this)

  override def toString() = "Rectangle is " + x + "," + y + "," + z

  def printShapeDescription() = {

    println("the shape is a rectangle and .......");
  }

  val shapeValue = "This is a Rectangle"
}