package com.thaj.functionalprogramming.shapeless

/**
 * Created by afsalthaj on 19/03/17.
 */
object ADT {
  /**
   * Algebraic data types (ADTs) are a functional programming concept with a fancy name but a very simple meaning.
   * They are an idioma􏰀c way of representingg data using “ands” and “ors”. For example
   * a shape is a rectangle or a circle
   * a rectangle has a width and a height • a circle has aradius
   * In ADT terminology, “and” types such as rectangle and circle are called products
   * and “or” types such as shape are called coproducts.
   * In Scala we typically represent products using case classes and coproducts using sealed traits
   */
  sealed trait Shape
  final case class Rectangle(width: Double, height: Double) extends Shape
  final case class Circle(radius: Double) extends Shape

  /** Alternate encodings:
   * While this encoding is less readable than the case class encoding above,
   * it does have some of the same desirable properties.
   */

  type Rectangle2 = (Double, Double)
  type Circle2    = Double
  type Shape2     = Either[Rectangle2, Circle2]
  val rect2: Shape2 = Left((3.0, 4.0))
  val circ2: Shape2 = Right(1.0)

  /**
   * Importantly, Shape2 is a more generic encoding than Shape3.
   * Any code that operates on a pair of Doubles will be able to operate on a Rectangle2 and vice
   * For example, if we’re serializing data to disk, we don’t care about the difference between a pair
   * of Doubles and a Rectangle2. We just write two numbers and we’re done.
   * Shapeless gives us the best of both worlds: we can use friendly semantic
   * types by default and switch to generic representa􏰀ons when we want interoperability (more on this later).
   * However, instead of using Tuples and Either, shape- less uses its own data types to
   * represent generic products and coproducts. We’ll introduce these types in the next sections.
   */

}
