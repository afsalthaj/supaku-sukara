package com.thaj.functionalprogramming.exercises.part3

import com.thaj.functionalprogramming.exercises.part3.Monad.{Functor, Monad}

import scala.{Right => _}

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

  trait Applicative[F[_]] extends Functor[F] { self =>
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

    // Exercise 12.8
    // Just like we can take the product of two monoids A and B to give the monoid (A, B),
    // we can take the product of two applicative functors. Implement this function:
    // Refer product
    def product[G[_]](B: Applicative[G]): Applicative[({type f[X] = (F[X], G[X])})#f] = {
      new Applicative[({type f[x] = (F[x], G[x])})#f] {
        def map2[A, B, C](fa: (F[A], G[A]), fb: (F[B], G[B]))(f: (A, B) => C): (F[C], G[C]) = {
          val fc = self.map2(fa._1, fb._1)(f)
          val gc = B.map2[A, B, C](fa._2, fb._2)(f)
          (fc, gc)
        }

         def unit[A](a: => A): (F[A], G[A]) = (self.unit(a), B.unit(a))
      }
    }

    /**
      * Please note that the order of exercise is not in line with the conceptual explanations. Please
      * make sure you are getting to the next topic, if you getting stuck with some of the exercises and solutions
      * in this project
      */
    // Exercise 12.9
    // Hard: Applicative functors also compose another way! If F[_] and G[_] are applicative functors,
    // then so is F[G[_]]. Implement this function:
    def compose[G[_]](G: Applicative[G]): Applicative[({type f[x] = F[G[x]]})#f] =
      new Applicative[({type f[x] = F[G[x]]})#f] {
        def lift[A, B, C](f: (A, B) => C) : (G[A], G[B]) => G[C] = (a: G[A], b: G[B]) => G.map2(a, b)(f)
        def map2[A, B, C](a: F[G[A]], b: F[G[B]])(f: (A, B) => C): F[G[C]]  = self.map2(a, b)(lift(f))
        def unit[A](a: => A): F[G[A]] = self.unit(G.unit(a))
      }

    // Exercise 12.12
    // On the Applicative trait, implement sequence over a
    // Map rather than a List: def sequenceMap[K,V](ofa: Map[K,F[V]]): F[Map[K,V]]
    /**
      * But traversable data types are too numerous for us to write specialized sequence and traverse methods
      * for each of them. What we need is a new interface. We’ll call it Traverse
      */
    def sequenceMap[K, V](ofa: Map[K, F[V]]): F[Map[K, V]] = ofa.foldLeft(unit(Map[K, V]()))((acc, keyValue) => {
      map2(acc, keyValue._2)((map, value) =>  map.updated(keyValue._1, value))
    })
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

  // Using applicative, it is impossible to implement join or flatMap
  // So Monad is clearly adding some extra capabilities beyond Applicative.
  // But what exactly? Let’s look at some concrete examples.

  /**
   * Why applicative trait?
   *  In general, it’s preferable to implement combinators like traverse using as few assumptions as possible.
   *  It’s better to assume that a data type can provide map2 than flatMap. Otherwise we’d have to write a new traverse
   *  every time we encountered a type that’s Applicative but not a Monad! We’ll look at examples of such types next.
   *
   *  Because Applicative is “weaker” than Monad, this gives the interpreter of applica- tive effects more flexibility.
   *  To take just one example, consider parsing. If we describe a parser without resorting to flatMap, this implies that the
   *  structure of our grammar is determined before we begin parsing. Therefore, our inter- preter or runner of parsers has more
   *  information about what it’ll be doing up front and is free to make additional assumptions and possibly use a more effi- cient
   *  implementation strategy for running the parser, based on this known structure. Adding flatMap is powerful, but it means we’re
   *  generating our pars- ers dynamically, so the interpreter may be more limited in what it can do. Power comes at a cost.
   *
   *  Applicative functors compose, whereas monads (in general) don’t.
   */

  // Not all applicative functors are monads

  // The idea behind this Applicative is to combine corresponding elements via zipping.
  val streamApplicative = new Applicative[Stream ] {
    def unit[A](a: => A): Stream[A] = Stream.continually(a)
    def map2[A, B, C](a: Stream[A], b: Stream[B])(f: (A, B) => C): Stream[C] = a zip b map(f.tupled)
  }

  // Exercise 12.5
  def eitherMonad[M] = new Monad[({ type f[X] = Either[M, X]}) #f] {
    def unit[A](a: => A): Either[M, A] = Right[M, A](a)
    def flatMap[A, B](ma: Either[M, A])(f: A => Either[M, B]): Either[M, B] = ma match {
      case Right(a) => f(a)
      case Left(x) => Left(x)
    }
  }

  /**
    * Dependency with flatMap has an inherent problem  that, after the first execution, it it fails, the second expression
    * is not called. During these instances, applicatives can be a solution.
    * {{{
    * map3(
        validName(field1),
        validBirthdate(field2),
        validPhone(field3))(
        WebForm(_,_,_))
    * }}}
    *
    * Here, no dependency is implied between the three expressions passed to map3, and in principle we can
    * imagine collecting any errors from each Either into a List. But if we use the Either monad, its
    * implementation of map3 in terms of flatMap will halt after the first error.
    */

  sealed trait Validation[+E, +A]

  case class Failure[E](head: E, tail: Vector[E] = Vector()) extends Validation[E, Nothing]

  case class Success[A](a: A) extends Validation[Nothing, A]

  /**
    * Write an Applicative instance for Validation that accumulates errors in Failure.
    * Note that in the case of Failure there’s always at least one error, stored in head.
    * The rest of the errors accumulate in the tail.
    */
  // EXERCISE 12.6
  // Refer to page number 212 and 213
  def validationApplicativeInstance[E] = new Applicative[({ type f[A] = Validation[E, A] }) #f]{
     def map2[A, B, C](fa: Validation[E, A], fb: Validation[E, B])(f: (A, B) => C): Validation[E, C] =
       fa match {
         case Success(a) => fb match {
           case Success(b) => Success(f(a, b))
           case Failure(t, ts) => Failure(t, ts)
         }

         case Failure(failure, failures) =>  fb match {
           case Success(_) => Failure(failure, failures)
           case Failure(fail, fails) => Failure(failure, fails ++ Vector(fail) )
         }
     }

    def unit[A](a: => A): Validation[E, A] = Success(a)
  }

  // Applicative Law

  // Identity Law
  /**
    * The first two laws for Applicative might be summarized by saying that both these implementations of map respect the
    * functor laws. In other words, map2 of some fa: F[A] with unit preserves the structure of fa. We’ll call these the
    * left and right identity laws (shown here in the first and second lines of code, respectively):
    * map2(unit(()), fa)((_,a) => a) == fa
    * map2(fa, unit(()))((a,_) => a) == fa
    */

  // Associativity
  /**
    * def product[A,B](fa: F[A], fb: F[B]): F[(A,B)] = map2(fa, fb)((_,_))
    * def assoc[A,B,C](p: (A,(B,C))): ((A,B), C) = p match { case (a, (b, c)) => ((a,b), c) }
    * product(product(fa,fb),fc) == map(product(fa, product(fb,fc)))(assoc)
    */

  // Naturality of product
  /**
    * When working with Applicative effects, we generally have the option of applying transformations before or
    * after combining values with map2.
    */

  // Hard: Prove that all monads are applicative functors by showing that if the
  // monad laws hold, the Monad implementations of map2 and map satisfy the applicative laws.
  // Let us believe so.
}