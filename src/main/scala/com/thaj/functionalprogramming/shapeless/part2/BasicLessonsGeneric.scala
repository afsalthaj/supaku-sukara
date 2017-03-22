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

  import shapeless.{Coproduct, :+:, CNil, Inl, Inr}
  case class Red(x: String)
  case class Amber()
  case class Green()
  type Light = Red :+: Amber :+: Green :+: CNil

  /**
   * In general coproducts take the form A :+: B :+: C :+: CNil meaning “A or B or C”,
   * where :+: can be loosely interpreted as Either. The overall type of a coproduct encodes
   * all the possible types in the disjunc􏰀on, but each con- crete instance contains a value for just
   * one of the possibili􏰀es. :+: has two subtypes, Inl and Inr, that correspond loosely to Left and Right.
   * We create instances of a coproduct by nes􏰀ng Inl and Inr constructors:
   */

  val red: Light = Inl(Red("x"))

  val green: Light = Inr(Inr(Inl(Green())))

  val amber: Light = Inr(Inl(Amber()))

  // Switching encodings using Generic

  /**
   * Coproduct types are difficult to parse on first glance.
   * However, we can see how they fit into the larger picture of generic encodings.
   * In addi􏰀on to un- derstanding case classes and case objects, shapeless’ Generic type class also
   * understands sealed traits and abstract classes:
   */
  import shapeless.Generic
  sealed trait Shape
  final case class Rectangle(width: Double, height: Double) extends Shape
  final case class Circle(radius: Double) extends Shape

  val gen = Generic[Shape]
  // gen: shapeless.Generic[Shape]{type Repr = shapeless.:+:[Rectangle,
  //shapeless.:+:[Circle,shapeless.CNil]]} = anon$macro$1$1@20ec902e

  gen.to(Rectangle(3.0, 4.0))
  // res3: gen.Repr = Inl(Rectangle(3.0,4.0))

  gen.to(Circle(1.0))
  // res4: gen.Repr = Inr(Inl(Circle(1.0)))
}