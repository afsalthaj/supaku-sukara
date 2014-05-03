package com.scalabasics.collections

import java.util.logging.Logger


/**
 * this covers three basic concepts:
 * 1. Tuple in Scala
 * 2. basic for loop
 * 3. Yielding values from a for loop (collecting values returned from a for loop to a variable)
 * 3. Providing conditional statements within a for loop
 * 3. Iterating over a tuple
 */
object IterateOverTuple {

  
  def main(args: Array[String]) {

    val tupleTobeSwapped = (2, 3)
    println("swapped tuple")
    println(tupleTobeSwapped.swap)

    val a = 0;
    val t = new Tuple3(2, 3, 4)
    
    //basic for loop
    println("basic for loop output")
    for(a<- 1 to 10){
      print (a+",")
    }
    // yielding an iterating return value from a for loop - just showing an example of how it can be used
    val returnVal = for { a <- 0 to 10 if a < 3 } yield a
    
   
    println("\niterating over a tuple")

    // iterating over the above returnVal again and then getting each value from the tuple
    for (a <- returnVal) {
    	// fetching the elements in a tuple with index a
      print(t.productElement(a)+",")
    }

  }
}