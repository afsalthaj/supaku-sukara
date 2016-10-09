package com.scalabasics.traitsexamples

import com.scalabasics.concurrency.Point


object UseRectangle {
  
  def main(args: Array[String]) {
  
    val rectangleInstance = new Rectangle (new Point(1,2), 3,4)
    
    rectangleInstance.printShapeDescription
    rectangleInstance.draw
    rectangleInstance.printMyShapeValue
    
    
}

}