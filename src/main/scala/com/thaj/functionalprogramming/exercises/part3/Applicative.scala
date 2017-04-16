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
    // Exercise 12.1
    def sequence[A](lfa: List[F[A]]): F[List[A]] = traverse(lfa)(fa => fa)
    def replicateM[A](n: Int, fa: F[A]): F[List[A]] = sequence(List.fill(n)(fa))
    def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] = map2(fa, fb)((_, _))
  }

  // Hard: The name applicative comes from the fact that we can formulate the Applicative
  // interface using an alternate set of primitives, unit and the function apply, rather than unit and map2.
  // Show that this formulation is equivalent in expressiveness by defining map2 and map in terms of unit and apply.
  // Also establish that apply can be implemented in terms of map2 and unit.

  // EXERCISE 12.2
  trait RealApplicative[F[_]] extends Functor[F] {
    def apply[A,B](fab: F[A => B])(fa: F[A]): F[B]
    def unit[A](a: => A): F[A]


    def map[A, B](fa: F[A])(f: A => B): F[B] = apply(unit[A => B](f))(fa)


    def map2[A,B,C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = apply(apply(unit[A => B => C](f.curried))(fa))(fb)

    def applyUsingMap[A, B](fab: F[A => B])(fa: F[A]): F[B] = map2(fa, fab)((a, ab) => ab(a))

    // Exercise 12.3
    // The apply method is useful for implementing map3, map4, and so on, and the pattern is straightforward.
    // Implement map3 and map4 using only unit, apply, and the curried method available on functions
    def map3[A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] = {
      val curriedBox: F[A => (B => C => D)] = unit(f.curried)
      apply(apply(apply(curriedBox)(fa))(fb))(fc)
    }

    def map4[A,B,C,D,E](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(f: (A, B, C, D) => E): F[E] = {
      val curriedBox: F[A => (B => C => D => E)] = unit(f.curried)
      apply(apply(apply(apply(curriedBox)(fa))(fb))(fc))(fd)
    }
  }
}