package com.thaj.functionalprogramming.example.exercises

/**
  * Created by afsalthaj on 22/10/2016.
  */
object EitherOperations {

  // Different from Options in the sense that, we give value to the reason for failure. (exceptions/errors)
  // You might ask why there should be two type parameters to track error.
  // The answer is quite straight forward.
  // Assume that actual value is of type A. It doesn't really make sense if the
  // same type parameter A is used to represent
  // the value of Error. So Either has E and A as type parameters.
  // Similar to options, it is a disjoint union of two data constructors.
  // However, two data constructors must give a proper type in place for E and A, with one of them
  // is always of the type `Nothing`
  // Exercise 4.6
  sealed trait Either[+E, +A] {

    def isLeft: Boolean

    def isRight: Boolean

    def map[B](f: A => B): Either[E, B] = this match {
      case Right(e) => Right(f(e))
      case Left(x) => Left(x)
    }

    // when mapping over the right side, we must promote the left side to some super type
    // to satisfy the +E variance annotation
    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
      case Right(e) => f(e)
      case Left(a) => Left(a)
    }

    def orElse[EE >: E, B](f: => Either[EE, B]) = if (isLeft) f else this
    def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
      this.flatMap(aa => b.map(bb => f(aa, bb)))
  }

  case class Left[+E](value: E) extends Either[E, Nothing] {
    def isLeft = true
    def isRight = false
  }

  case class Right[+A](value: A) extends Either[Nothing, A] {
    def isLeft = false
    def isRight = true
  }

  // Exercise 4.7
  object Either {
    def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = {
      es.foldRight(Right(Nil): Either[E, List[A]])((a, b) => a.map2(b)( _ :: _))
    }

    def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]]  =
      as.foldRight(Right(Nil): Either[E, List[B]])((a,b) =>f(a).map2(b)( _ :: _))

    def Try[A](a: => A): Either[String, A] =
      try {
        Right(a)
      } catch {
        case e: Exception => Left(e.getMessage)
      }
  }
}

object EitherExamples {
  import EitherOperations._, Either._
  // A new type: IndexedSeq. This explanation has nothing to do with
  // the concepts that we try to cover here.
  // In Java terms, Scala's Seq would be Java's List,
  // and Scala's List would be Java's LinkedList.
  // Seq is an interface or trait (java or scala). So while java's list
  // is an interface, Scala's list is a concrete implementation, implemented by
  // :: and Nil. Scala's List is more or less Java's Linked List. But the main difference
  // between Scala's List and Java's Linked List is that scala.List is immutable and Java's list
  // is immutable. Scala's List is highly optimized by compiler and libraries, and it's a
  // fundamental data type in functional programming. However, it has limitations and it's inadequate
  // for parallel programming.
  // These days, Vector is a better choice than List, but habit is hard to break.
  // Seq is a good generalization for sequences, so if you program to interfaces, you should use that.
  // Note that there are actually three of them: collection.Seq, collection.mutable.Seq
  // and collection.immutable.Seq, and it is the latter one that is the "default" imported into scope.

  def mean (xs: IndexedSeq[Double]): Either[String, Double] =
    if (xs.isEmpty)
      Left("mean of empty list")
    else
      Right(xs.sum/xs.length)

  def safeDiv(x: Int, y: Int): Either[Exception, Int] =
    try
      Right(x / y)
    catch {
      case e: Exception => Left(e)
    }

  def insuranceRateQuote (age: Int, numberOfSpeedingTickets: Int) =
    age+numberOfSpeedingTickets


  def parseInsuranceQuotes(age: String, numberOfSpeedingTickets: String): Either[String, Int] = {
    val ageInt = Try {age.toInt}
    val numberOfSpeedingTicketsInt = Try {numberOfSpeedingTickets.toInt}

    ageInt.map2(numberOfSpeedingTicketsInt)(insuranceRateQuote)
  }

  def parseInsuranceQuotesUsingForComprehension(age: String, noT: String):  Either[String, Int]  = {
    for {
      ageInt <- Try{age.toInt}
      numberOfSpeedingTicket <- Try {noT.toInt}
    } yield insuranceRateQuote(ageInt, numberOfSpeedingTicket)
  }

  case class Person(name: Name, age: Age)
  sealed class Name(val value: String)
  sealed class Age(val value: Int)

  def mkName(name: String): Either[String, Name] =
    if (name == "" || name == null) Left("Name is empty.")
    else Right(new Name(name))

  def mkAge(age: Int): Either[String, Age] =
    if (age < 0) Left("Age is out of range.")
    else Right(new Age(age))

  def mkPerson(name: String, age: Int): Either[String, Person] =
    mkName(name).map2(mkAge(age))(Person(_, _))


  // In this implementation, map2 is only able to report one error,
  // even if both the name and the age are invalid. What would you need to change in order to report both errors?
  // Would you change map2 or the signature of mkPerson? Or could you create a new data type
  // that captures this requirement better than Either does, with some additional structure?
  // How would orElse, traverse, and sequence behave differently for that data type?

  /*
There are a number of variations on `Option` and `Either`. If we want to accumulate multiple errors, a simple
approach is a new data type that lets us keep a list of errors in the data constructor that represents failures:

trait Partial[+A,+B]
case class Errors[+A](get: Seq[A]) extends Partial[A,Nothing]
case class Success[+B](get: B) extends Partial[Nothing,B]

There is a type very similar to this called `Validation` in the Scalaz library. You can implement `map`, `map2`,
`sequence`, and so on for this type in such a way that errors are accumulated when possible (`flatMap` is unable to
accumulate errors--can you see why?). This idea can even be generalized further--we don't need to accumulate failing
values into a list; we can accumulate values using any user-supplied binary function.

It's also possible to use `Either[List[E],_]` directly to accumulate errors, using different implementations of
helper functions like `map2` and `sequence`.
*/

}

// Based on the last few comments => EXERCISE 4.8
sealed trait Validation[+E, +A] {
  def map[B](f: A => B): Validation[E, B] = this match {
    case Success(a) => Success(f(a))
    case Error(Seq(a)) => Error(Seq(a))
  }

  def flatMap[EE >: E,B](f: A => Validation[EE,B]): Validation[EE, B] = this match {
    case Success(a) => f(a)
    case Error(errors) => Error(errors)
  }

  def map2[EE >: E, B, C](b: Validation[EE, B])(f: (A, B) => C): Validation[EE, C] = {
    (this, b) match {
      case (Success(a), Success(c)) => Success(f(a, c))
      case (Error(a), Success(c)) => Error(a)
      case (Success(a), Error(c)) => Error(c)
      case (Error(a), Error(c)) => Error(c ++ a)
    }
  }
}

case class Error[+E](get: Seq[E]) extends Validation[E, Nothing]

case class Success[+A](get: A) extends Validation[Nothing, A]

// The implementation of sequence and traverse is similar to
// that of Either and Option. But the engine that works
// behind these implementation, that is `map2` differs here.
// `map2` of Validation accumulates error, where as
// map2 of Option and Error don’t do that.

object Validation {
  def Try[A](a: => A): Validation[String, A] =
    try {
      Success(a)
    } catch {
      case e: Exception => Error(Seq(e.getMessage))
    }

  def sequence[E, A](a: List[Validation[E, A]]): Validation[E, List[A]] = {
    a.foldRight(Success(Nil): Validation[E, List[A]])((a, b) => a.map2(b)(_ :: _))
  }

  def traverse[E, A, B](a: List[A])(f: A => Validation[E, B]): Validation[E, List[B]] =
    a.foldRight(Success(Nil): Validation[E, List[B]])((a, b) => f(a).map2(b)(_ :: _))
}


