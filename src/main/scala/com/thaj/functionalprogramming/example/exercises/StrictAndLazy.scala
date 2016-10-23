package com.thaj.functionalprogramming.example.exercises

/**
  * Created by afsalthaj on 23/10/2016.
  */

import scala.{Stream => _}

sealed trait Stream[+A]

case object Empty extends Stream[Nothing]

// A non empty stream consist of a head and a tail, which are both non-strict.
// Due to technical limitations, these are thunks that must be explicitly forced,
// rather than by-name parameters.
case class Cons[+A](head: () => A, tail: () => Stream[A]) extends Stream[A]

object Stream {
  // smart Constructors
  def cons[A](a:() => A, t: () => Stream[A]): Stream[A]  = {
    // so, Cons is created through cons function, which is basically a smart constructor
    // this will ensure that head and tail are evaluated just once.. yes only when the very first time
    // it is called.
    lazy val head = a()
    lazy val tail = t()

    Cons(() => head, () => tail)
  }

  def apply[A](as: A*): Stream[A] = {
    if(as.isEmpty) Empty else Cons(() => as.head, () => apply(as.tail: _*))
  }
}