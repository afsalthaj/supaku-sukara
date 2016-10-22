package com.thaj.functionalprogramming.example.exercises

/**
  * Created by afsalthaj on 20/10/2016.
  */

object OptionOperation {
  sealed trait Option[+A] {
    // a handy get function
    def get: A

    def isEmpty: Boolean

    def map[B](f: A => B): Option[B] = this match {
      case Some(a) => Some(f(a))
      case None => None
    }


    def flatMap[B](f: A => Option[B]): Option[B] =
      if (isEmpty) None else f(this.get)

    // We know Nothing is a subtype of everthing.
    // this is why val s: List[String] = Nil works.
    // Nil means A is Nothing. hence if A is a subtype of B,
    // then Option[A] is a subtype of Option[B]
    // in order to keep variance B is a super type of A
    // or , A must be equal to B, or B must be greater than A.
    // Somethhg like Lion should be tagged as a Lioj, or atleast
    // tagged as Animal. Also default will get executed only if the condition is valid
    def getOrElse[B >: A](default: => B): B = if (isEmpty) default else this.get

    // If the option is empty then I need another option (this is not getOrElse).. It is orElse
    def orElse[B >: A](ob: => Option[B]): Option[B] = if (isEmpty) ob else this

    //filter
    def filter(f: A => Boolean): Option[A] = if (isEmpty || f(this.get)) this else None
  }

  case class Some[A](get: A) extends Option[A] {
    def isEmpty = false
  }

  case object None extends Option[Nothing] {
    def isEmpty: Boolean = true

    def get = throw new NoSuchElementException("None.get")
  }

  object Option {
    import com.thaj.functionalprogramming.example.exercises. { TypeAndDataConstructors => My }

    //Exercise 4.4
    // Write a function sequence that combines a list of Options into one Option
    // containing a list of all the Some values in the original list. If the original list contains None
    // even once, the result of the function should be None; otherwise the result should be Some with a list of all the values. Here is its signature:[3]
    // This is a clear instance where it’s not appropriate to define the function in the OO style.
    // This shouldn’t be a method on List (which shouldn’t need to know anything about Option), and it can’t be a method on Option, so it goes in the Option companion object.

    def sequence[A](a: List[Option[A]]): Option[List[A]] = {
      a.foldRight(Some(Nil): Option[List[A]])((a, b) => Exercise.map2(a, b)(_ :: _))
    }
  }
}


object Exercise {
  import OptionOperation._
  //Exercise 4.2
  //Implement the variance function in terms of flatMap.
  // If the mean of a sequence is m, the variance is the mean of math.pow(x - m, 2)
  // for each element x in the sequence
  def variance(xs: Seq[Double]): Option[Double] = {
    def mean(seq: Seq[Double]) = if(xs.isEmpty) None else Some(xs.sum/xs.length)
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))
  }

  // lift - lifts an unary function to operate on optional values
  def lift[A, B](f: A => B): Option[A] => Option[B] = (t: Option[A]) => t.map(f)
  // lift in FP book
  def liftInBook[A, B](f: A => B): Option[A] => Option[B] = _ map f

  // Similar to Lift1 it lifts a binary function to operate with optional values
  def lift2[A, B, C](f: (A, B) => C): (Option[A], Option[B]) => Option[C] =
    (a: Option[A], b: Option[B]) => a.flatMap(aa => b.map(bb => f(aa, bb)))

  // Exercise 4.3
  // Write a generic function map2 that combines two Option values
  // using a binary function. If either Option value is None,
  // then the return value is too. Here is its signature:
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a.flatMap(aa => b.map(bb => f(aa, bb)))

  // An example for using map2, or lift2
  def insuranceRateQuote(age: Int, numberOfSpeedingTickets: Int): Double = {
    age + numberOfSpeedingTickets
  }

  def parseInsuranceRateQuote(age: String, numberOfSpeedingTickets: String): Option[Double] = {
    val ageInt: Option[Int] = Try { age.toInt }
    val numberOfSpeedingTicketsInt = Try { numberOfSpeedingTickets.toInt }
    // lift2(insuranceRateQuote)(ageInt, numberOfSpeedingTicketsInt)
    map2(ageInt,numberOfSpeedingTicketsInt)(insuranceRateQuote)
  }

  // A Try function that converts exeception throwing operations to Option
  // Remember it has argument which is non strict and lazy, this means the potentially
  // exception throwing operation `a` executes only inside Try
  def Try[A] (a: => A) = try Some(a) catch {case e: Exception => None }

  // another example of using lift
  def getMathAbs(x: Option[Int]) = lift (math.abs)(x)

  // This is unfair; 2 times parsing the list.
  def parseInts(a: List[String]): Option[List[Int]] = {
    val list: List[Option[Int]] = a.map(t => Try(t.toInt))
    Option.sequence(list)
    // Option.sequence(a map (i => Try(i.toInt)))
  }

  //traverse came in for solution, taking a list of A..a map that takes each
  //element and give List[Option], but the end result is Option[List]
  // this doesn't give much advantage, we parsed the list twice
   def traverseNotEfficient[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
   val list: List[Option[B]] = a.map(f)
    Option.sequence(list)
  }

  // Exercise 4.5
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    a.foldRight(Some(Nil): Option[List[B]])((x, y) => map2(f(x), y)( _ :: _))
  }

  def parseIntInTermsOfTraverse(a: List[String]): Option[List[Int]] =
    traverse(a)( t => Try {t.toInt})

  def sequenceInTermsOfTraverse[A](a: List[Option[A]]): Option[List[A]] =
    traverse(a)(identity)
}


// Option summary
/**
If you have a list of A, and you have an operation that takes each element in this list and returns a Option,
but your expected return type is
option of list, and not list of option, then you can call traverse function with the list, function A => Option[B]
  {{{

  def traverse[A] (list: List[A])(f: A => Option[B]): Option[List[B]] = {
    list.foldRight(Some(Nil): List[Option[B]])((a, b) => map2(f(x), y) (_ :: _))
  }
  }}}

If you have a list of option[A] and you want a option of list[B], call sequence function

  {{{
    def sequence[A](list: List[Option[A]]): Option[List[A]] =
     traverse(list)(identity)
  }}}


If you have two options, but your operation is based on two values and not options, then
you pass your operation and the two options to map2 function, so that
  {{{
   def map2[A, B](a: Option[A], b: Option[B]) (f: (A, B) => C): Option[C] =
    a.flatMap(aa => b.map(bb => f (aa, bb)))
  }}}

 If you have a function that operates only on values, and you want them to act on options
  it is fairly straight forward
  {{{
    def lift(f: A => B): Option[A] => Option[B] = {
      _ map f
     //(c: Option[A]) => c.map(f(a))
    }
  }}}

  While you are familiar with the above sophisticated methodologies of approaching a problem,
  bear in mind that everything can be solved using map, flatMap, filter, getOrElse, orElse etc


  sequence, lift etc is not just about option, so it may not be defined in trait Option, but it can be in
  its companion object
**/


/**
  * For Comprehensions:
  * Lifting operations is what we have discussed here mostly.
  * Be it traverse, sequence, map2, lift or lift2, it is all lift operations
  *  {{{
  *    def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
        a flatMap (aa => b map (bb => f(aa, bb)))
  *  }}}
  *
  * same can be done using
  *  {{{
  *    for {
  *      aa <- a
  *      bb <- b
  *    } yield f(aa, bb)
  *  }}}
  *
  *  Between map, lift, sequence, traverse, map2, map3, and so on,
  *  you should never have to modify any existing functions to work with optional values.
  */
