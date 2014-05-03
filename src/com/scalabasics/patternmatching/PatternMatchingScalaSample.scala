package com.scalabasics.patternmatching

/**
 * another set of different pattern matching
 */
object PatternMatchingScala1 {

  def main(args: Array[String]) {

    val sundries = List(23, "hello", 8.5, 'q')

    for (sundry <- sundries) {
      sundry match {
        case i: Int => println("got an integer value")
        case other => println("uff")
      }

    }

    val list1 = List(12, 23, 45)
    val willNotWork = List(4, 8, 52)
    val empty = List()
    for (l <- List(list1, willNotWork, empty)) {

      l match {
        
        case List(_, 3, _, _) => println("four elements with second element 3")
        case List(_*) => println("Any other list with 0 or more elements")

      }
    }
    
    val tupA = ("Good","Morning")
    val tupB= ("Guten","Tag")
    for(tup <-List(tupA,tupB)){
      
      tup match {
        //guarding
        case (thingOne,thingTwo) if thingOne == "Good" =>println("A two tuple starting with goood")
        case (thingOne,thingTwo) => println("This tuple has 2 things")
      }
    }
  }

}