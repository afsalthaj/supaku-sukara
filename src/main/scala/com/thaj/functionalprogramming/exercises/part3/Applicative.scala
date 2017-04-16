package com.thaj.functionalprogramming.exercises.part3

import com.thaj.functionalprogramming.exercises.part3.Monad.Functor

object Applicative {
  // If you look at `combinators` defined in Monad, most of them are defined in terms of
  // map2 and unit, for example, traverse is such a function
  // For many data types map2 can be implemented directly without the help of flatMap
  // This means the below code base may not be always the case:
  /**
    * // For monad F
    *
    * {{{
    *   def traverse[A, B](a: List[A])(f: A => F[B]]): F[List[B] = {
    *     a.foldLeft(unit(List[B]())((a, b) => map2(f(a), b))( _ :: _)
    *   }
    * }}}
    */
  // Sometimes we may get confused that map2 and unit can be the primitives and rest everything is combinators
  // because of its usage.
  // This leads to another interface, where map2 and unit can be the primitives and rest everything is combinators
  // This is known as applicative functors, but the less powerful nature of applicative functors comes with benefits.

  trait Applicative[F[_]] extends Functor[F] {
    def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C]
    def unit[A](a: => A): F[A]

    // derived combinators
    def map[A, B](a: F[A])(f: A => B): F[B] = map2(a, unit(()))((a, _) => f(a))
    def traverse[A, B](a: List[A])(f: A => F[B]): F[List[B]] =
      a.foldLeft(unit(List[B]()))((a, b) => map2(f(b), a)(_ :: _))

    /**
      * Note that the implementation of traverse is unchanged.
      * We can similarly move other combinators into Applicative that don’t depend directly on flatMap or join
      */
    def sequence[A](lfa: List[F[A]]): F[List[A]] = traverse(lfa)(fa => fa)
  }
}
