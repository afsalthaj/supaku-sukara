package com.scalabasics.concurrency

import scala.actors.Actor

/**
 * defining an actor. Actor is another concept in Scala
 * The basic framework of actor is as shown below; a loop that continuously accepts 
 * different types and matches with different cases. This class works with <UseActor> class
 * defined in the same package
 */
object ShapeDrawingActor extends Actor {

  def act() {
    loop {
      receive {
        case s: Shape => s.draw
        case "exit" => println("hoi exit")
        case x: Any => println("Error unknown message")
      }
    }
  }

}