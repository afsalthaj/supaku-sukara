package com.scalabasics.concurrency

/**
 * actor concept in Scala. 
 * The content after "!" will be sent to the ShapeDrawingActor
 */
object UseActor {
  
  def main(args: Array[String]) {
    
    ShapeDrawingActor.start
    
    ShapeDrawingActor ! new Triangle (new Point(1,2),new Point(3,4),new Point(4,5))
    
    ShapeDrawingActor ! new Rectangle (new Point (2,3),4,5)
    
    ShapeDrawingActor ! "exit"
  
}
  

}