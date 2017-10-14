package com.scalabasics.patternmatching

/**
 * simple case matching - different types
 */
object MatchingCaseClassSamples {

  def main(args: Array[String]) {

    case class Person(name: String, age: Int)

    val alice = new Person("Alice", 25)
    val bob = new Person("bob", 32)
    val charlie = new Person("Charlie", 32)

    for (person <- List(alice, bob, charlie)) {

      person match {
        case Person("Alice", 25) => println("Hi Alice")
        case Person("bob", 32)   => println("Hi Bob")
        case Person(name, age)   => println("Who are you " + age + " year old person name " + name)

      }
    }

    val list = List("Afsal", 1, "2", 'c')

    for (hi <- list) {
      hi match {

        case n: Int    => println("its an integer")
        case s: String => println("its a string")
        case l: Char   => println("its a char")
        case others    => println("what is this")
      }
    }
  }

}