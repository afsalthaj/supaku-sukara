package com.thaj.functionalprogramming.shapeless.part1

/**
 * Created by afsalthaj on 19/03/17.
 */
/**
 * Generic programming
 *
 *  def employeeCsv(e: Employee): List[String] =
 * List(e.name, e.number.toString, e.manager.toString)
 * def iceCreamCsv(c: IceCream): List[String] =
 * List(c.name, c.numCherries.toString, c.inCone.toString)
 * Generic programming is about overcoming differences like these.
 * Shapeless makes it convenient to convert specific types into generic ones that we can manipulate with common code.
 *
 */
object GenericProgramming {

  case class Employee(name: String, number: Int, manager: Boolean)
  case class IceCream(name: String, numCherries: Int, inCone: Boolean)

  def employeeCsv(e: Employee): List[String] =
    List(e.name, e.number.toString, e.manager.toString)
  def iceCreamCsv(c: IceCream): List[String] =
    List(c.name, c.numCherries.toString, c.inCone.toString)

  import shapeless._
  val genericEmployee = Generic[Employee].to(Employee("Dave", 123, false))
  // genericEmployee: shapeless.::[String,shapeless.::[Int,shapeless.::[
  // Boolean,shapeless.HNil]]] = Dave :: 123 :: false :: HNil
  val genericIceCream = Generic[IceCream].to(IceCream("Sundae", 1, false))
  // genericIceCream: shapeless.::[String,shapeless.::[Int,shapeless.::[
  // Boolean,shapeless.HNil]]] = Sundae :: 1 :: false :: HNil
}

// Referred from shapeless guide
object ADT {
  // Chapter 2 - 2.1
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

  /**
   * Alternate encodings:
   * While this encoding is less readable than the case class encoding above,
   * it does have some of the same desirable properties.
   */

  type Rectangle2 = (Double, Double)
  type Circle2 = Double
  type Shape2 = Either[Rectangle2, Circle2]
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

  // Generic product Encodings
  /**
   * Tuples have issues:
   * 1) 0 element tuple doesn't have a tuple. You could replace it with Unit, but ideally generic representations
   * should have a common supertype. The least super type of Tuple2 and Unit is Any, which is not ideal.
   * 2) Each size of tuple has a different, unrelated type, making it difficult to write code that abstracts over sizes
   *
   * For all these reasons, shapeless uses a different generic encoding for product types called heterogeneous lists
   * or HLists4.
   *
   * Code base from shapeless:
   *
   * sealed trait HList extends Product with Serializable
   *
   * final case class ::[+H, +T <: HList](head : H, tail : T) extends HList {
   * ......
   * }
   *
   * sealed trait HNil extends HList
   *
   * Because every :: has its own H and T, the type of each element is encoded separately in the type of the overall list:
   *
   */

  import shapeless.{ ::, HNil }

  val product: String :: Int :: Boolean :: HNil = "Sunday" :: 1 :: false :: HNil

  val first = product.head
  // first: String = Sunday
  val second = product.tail.head
  // second: Int = 1
  val rest = product.tail.tail
  // rest: shapeless.::[Boolean,shapeless.HNil] = false :: HNil
  // product.tail.tail.tail.head throws compilation error

  /**
   * he behaviour we get from HLists isn’t magic. We could have achieved all of this
   * func􏰀onality using (A, B) and Unit as alterna􏰀ves
   * to :: and HNil. How- ever, there is an advantage in keeping our representa􏰀on types
   * separate from the seman􏰀c types used in our applica􏰀ons. HList provides this separa􏰀on.
   */
}
