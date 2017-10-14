package com.scalabasics.traitsexamples

/**
 * search for mix in composition
 */
trait TraitSample {

  val shapeValue: Any

  def printShapeDescription(): Unit
  def printMyShapeValue() = {
    println("hai " + shapeValue)
  }

}