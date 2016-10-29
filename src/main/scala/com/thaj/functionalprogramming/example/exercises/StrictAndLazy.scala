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

}

case object Empty extends Stream[Nothing]

// A non empty stream consist of a head and a tail, which are both non-strict.
// Due to technical limitations, these are thunks that must be explicitly forced,
// rather than by-name parameters.
case class Cons[+A] private (head: () => A, tail: () => Stream[A]) extends Stream[A]

object Stream {
  // smart Constructors
  def cons[A](a: => A, t: => Stream[A]): Stream[A]  = {
    // so, Cons is created through cons function, which is basically a smart constructor
    // this will ensure that head and tail are evaluated just once.. yes only when the very first time
    // it is called.
    // We cache the head and tail as lazy values to avoid repeated evaluation
    lazy val head = a
    lazy val tail = t

    Cons(() => head, () => tail)
  }

  // A smart constructor to create empty stream of a particular type
  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] = {
    if(as.isEmpty) Empty else Cons(() => as.head, () => apply(as.tail: _*))
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