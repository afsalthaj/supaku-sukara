package com.scalabasics.concurrency

/**
 * class Point which has function that overrides toString method
 */
class Point(val x: Double, val y: Double) {

  override def toString() = "Point ( " + x + "," + y + ")"

}