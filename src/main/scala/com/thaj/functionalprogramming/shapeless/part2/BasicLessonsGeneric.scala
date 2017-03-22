package com.thaj.functionalprogramming.shapeless.part2

import shapeless._
/**
 * Created by afsalthaj on 22/03/17.
 */


object BasicLessonsGeneric {
// PRODUCTS
  /**
   * Shapeless provides a type class called Generic that allows us to switch back and forth between a concrete ADT
   * and its generic representation. Some behind-the-scenes macro magic allows us to
   * summon instances of Generic without boilerplate:
   */


  case class IceCream(name: String, numCherries: Int, inCone: Boolean)
  val iceCreamGen = Generic[IceCream]
  // iceCreamGen: shapeless.Generic[IceCream]{type Repr = shapeless.::[
  // String,shapeless.::[Int,shapeless.::[Boolean,shapeless.HNil]]]} =
  // anon$macro$4$1@745fe7b1

  /**
   * Instances of Generic have two methods: one for conver􏰀ng to the Repr type and one for conver􏰀ng from it:
   */
  val iceCream = IceCream("Sundae", 1, false)
  // iceCream: IceCream = IceCream(Sundae,1,false)
  val repr = iceCreamGen.to(iceCream)
  // repr: iceCreamGen.Repr = Sundae :: 1 :: false :: HNil
  val iceCream2 = iceCreamGen.from(repr)
  // iceCream2: IceCream = IceCream(Sundae,1,false)

  /**
   * If two ADTs have the same Repr, we can convert back and forth between them with their Generics
   */
  case class Employee(name: String, number: Int, manager: Boolean)

  // Create an employee from icecream:
  // representing the ice-cream instance as a generic pattern
  val icecreamRepr = Generic[IceCream].to(iceCream)
  // converting the above generic repr to an an actual employee
  val employeeRepr = Generic[Employee].from(icecreamRepr)

  val tupleGen = Generic[(String, Int, Boolean)]

  val tupleRepr = tupleGen.to(("Hello", 123, true))
  // res4: tupleGen.Repr = Hello :: 123 :: true :: HNil

  val getMyTupleBack: (String, Int, Boolean) = tupleGen.from(tupleRepr)


  // CO-PRODUCTS
}
