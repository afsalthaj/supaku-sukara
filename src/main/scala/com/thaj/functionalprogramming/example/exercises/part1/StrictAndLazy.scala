package com.thaj.functionalprogramming.example.exercises

/**
  * Created by afsalthaj on 23/10/2016.
  */

import scala.{Stream => _}

sealed trait Stream[+A] {
  import Stream._
  // Exercise 5.1
  // Write a function to convert a Stream to a List,
  // which will force its evaluation and let you look at it in the REPL.
  // You can convert to the regular List type in the standard library.
  // You can place this and other functions that operate on a Stream inside the Stream trait.

  def toList: List[A] = {
    def go(stream: Stream[A], acc: List[A]): List[A] = {
      stream match {
        case Empty => acc
        case Cons(h, t) => go(t(), h() :: acc)
      }
    }
    go(this, Nil).reverse
  }

  // Exercise 5.2
  // Write the function take(n) for returning the
  // first n elements of a Stream, and drop(n)
  // for skipping the first n elements of a Stream.
  def take(n: Int): Stream[A] = this match {
    case Empty => Empty
    case Cons(h, t) if n<=0 => Empty
    case Cons(h, t) => Stream.cons(h(), t().take(n-1))
  }

  def drop(n: Int): Stream[A] = this match {
    case _ => this
    case Cons(h, t) if n > 0 => t().drop(n-1)
  }

  // Exercise 5.3
  def takeWhile(f: A => Boolean): Stream[A] = this match {
    case Cons(h,t) if f(h()) => cons(h(), t().takeWhile(f))
    case _ => empty
  }

  // More generally speaking, laziness lets us separate the description of an
  // expression from the evaluation of that expression.
  // This gives us a powerful ability—we may choose to describe a
  // “larger” expression than we need, and then evaluate only a portion of it. As an
  // example, let’s look at the function exists that checks whether an element
  // matching a Boolean function exists in this Stream:

  // Note that || is non-strict in its second argument.
  // If p(h()) returns true, then exists terminates the traversal early and returns true as well.
  // Remember also that the tail of the stream is a lazy val. So not only does the traversal terminate early,
  // the tail of the stream is never evaluated at all! So whatever code would have generated the tail
  // is never actually executed.
  def exists(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) || t().exists(p)
    case Empty => false
  }

  /**
    * Fold Right function of list is shown here
    * {{{
    *  def foldRight[A, B](list: List[A], init: B)(f: (A, B) => B): B = list match {
    *    case Nil => init
    *    case Cons(x, xs) => f(x, foldRight(xs, init)(f))
    *  }
    * }}}
    *
    * Similar foldRight for stream is shown here.
    * The function f takes its second argument by name and not value,
    * and may choose not to evaluate it.
    * If f doesn't evaluate its second argument, the recursion never occurs.
    * Now you may think, what is the use of this function, assuming that you
    * call foldRight with the assumption you will be parsing through the whole elements.
    * But this is not the case, a typical example is re-implementing exists method using foldRight.
    * exists function using the below method will ensure that method terminates early (faster) than
    * the list foldRight.
    */
  def foldRight[B](init: => B)(f: (A, => B) => B): B = this match {
    case Empty => init
    case Cons(h, t) => f(h(), t().foldRight(init)(f))
  }

  // Here b is the unevaluated recursive step that folds the tail of the stream. If p(a) returns true,
  // b will never be evaluated and the computation terminates early.
  def existsUsingFoldRight(f: A => Boolean): Boolean = this.foldRight(false)((a, b) => f(a) || b)

  // def forAll(p: A => Boolean): Boolean
  def forAll(p: A => Boolean): Boolean = this.foldRight(true)((a, b) => p(a) && b)

  //Exercise 5.5
  // takeWhile in terms of foldRigh
  def takeWhile_1 (f: A => Boolean): Stream[A] = this.foldRight(Empty: Stream[A])((a, b) =>
    if(f(a))
      cons(a, b)
    else
      empty
  )

  // Exercise 5.8
  /**Hard: Implement headOption using foldRight.
    {{{
    def headOption(a: Stream[A]): Option[A] = a match {
      case Cons(h, t) => Some(h())
      case Empty => None
    }
    }}}

    {{{
      def foldRight[B](init: => B)(f: (A, => B) => B): B = this match {
       case Empty => init
      case Cons(h, t) => f(h(), t().foldRight(init)(f))
    }
    But it seems, it traverses the whole elements in the stream to calculate the second
    element of the function, which is irrelevant here. Thats not good either
    */

  def headOption_1: Option[A] = this.foldRight(None: Option[A])((a, _) => Some(a))

  def append[B >: A](s: => Stream[B]): Stream[B] = foldRight(s)((a, b) => cons(a, b))
  // Exercise 5.7
  // Implement map, filter, append, and flatMap
  // using foldRight. The append method should be non-strict in its argument.
   def map[B](f: A => B): Stream[B] = foldRight(Empty: Stream[B])((a, b) => cons(f(a), b))
   def filter(f: A => Boolean): Stream[A] = foldRight(Empty: Stream[A])((a, b) => if(f(a)) cons(a, b) else b)
   def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(Empty: Stream[B])((a, b) => f(a) append b)

  // To assert a few things into your brain, have a look at the below function
  def mapUnfold[B](f: A => B): Stream[B]= this match {
    case Cons(h, t) => cons(f(h()), t().mapUnfold(f))
    case Empty => Empty
  }
  /**
    * Theory:
    *  Example used for explanation: Stream(1,2,3,4).map(_ + 10).filter(_ % 2 == 0)
    * The concept here is, be it any function which is part of the stream , `b` is never evaluated until
    * it is needed. You can forget about rest of the complexities. You know how stream is implemented, especially
    * using the smart constructor cons. It takes unevaluated A, and unevaluated B, and returns Cons(a, b) both being
    * unevaluated. Hence it is a stream. Now assume that you are using map function.
    *  What is happening with map. It is just a short description, or just a start of the stream which is
    *  represented here. Nothing is computed here, since cons by name parameters. that is the lazy head and tail
    *  are still lazy, and noone has called it yet. So you now are now applying a filter to it. The condition for filter
    *  is f(a), and obviously lazy val is executed but only for the head, and you may get a filtered stream now.
    *  Again, it tries to filter so that if(f(a)) is evaluated, and a being the second element now, and you get a filtered
    *  stream. This means the operation between map and filter is inter-leaved. the computation alternates between generating a
    *  single element of the output of map, and testing with filter to see if that element is
    *  divisible by 2 (adding it to the output list if it is). Note that we don’t fully instantiate the
    *  intermediate stream that results from the map. It’s exactly as if we had interleaved the logic using a
    *  special-purpose loop. For this reason, people sometimes describe streams as “first-class loops”
    *  whose logic can be combined using higher-order functions like map and filter.
    *  Since intermediate streams aren’t instantiated, it’s easy to reuse existing combinators in novel ways without having to worry that we’re doing more processing of the stream than necessary. For example, we can reuse filter to define find, a method to return just the first element that matches if it exists. Even though filter transforms the whole stream, that transformation is done lazily, so find terminates as soon as a match is found:
       {{{
       def find(p: A => Boolean): Option[A] =
        filter(p).headOption
       }}}
    */
}

case object Empty extends Stream[Nothing]

// A non empty stream consist of a head and a tail, which are both non-strict.
// Due to technical limitations, these are thunks that must be explicitly forced,
// rather than by-name parameters.
case class Cons[+A] (head: () => A, tail: () => Stream[A]) extends Stream[A]

object Stream {
  // smart Constructors
  def cons[A](a: => A, t: => Stream[A]): Stream[A]  = {
    // so, Cons is created through cons function, which is basically a smart constructor
    // this will ensure that head and tail are evaluated just once.. yes only when the very first time
    // it is called.
    // We cache the head and tail as lazy values to avoid repeated evaluation
    // We typically want to cache the values of a Cons node,
    // once they are forced. If we use the Cons data constructor directly,
    // for instance, this code will actually compute expensive(x) twice:
    // val x = Cons(() => expensive(x), tl)
    // val h1 = x.headOption
    // val h2 = x.headOption
    lazy val head = a
    lazy val tail = t

    Cons(() => head, () => tail)
  }

  // A smart constructor to create empty stream of a particular type
  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] = {
    if(as.isEmpty) Empty else cons(as.head, apply(as.tail: _*))
  }

  def headOption[A](a: Stream[A]): Option[A]= a match {
    case Empty => None
    // Explicit forcing of the h thunk using h()
    // Note that we have to force h explicitly via h(), but other than that,
    // the code works the same way as it would for List. But this ability of Stream to
    // evaluate only the portion actually
    // demanded (we don’t evaluate the tail of the Cons) is useful, as we’ll see.
    case Cons(h, t) => Some(h())
  }


}
