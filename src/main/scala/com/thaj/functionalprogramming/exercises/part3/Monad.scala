package com.thaj.functionalprogramming.exercises.part3

import com.thaj.functionalprogramming.example.exercises.part2.Gen

/**
 * Created by afsalthaj on 4/03/17.
 */
object Monad {

  /**
   *   In all our previous APIs, we ended up using primitive functions such as map, flatMap etc and corresponding combinators
   *   such as sequence, traverse etc. If you haven't got a sense of these functions yet, its good to have a revisit on part 1, part 2
   *   etc
   *
   *   We will call F (or F[_] as) type constructor.
   */

  /**
   *   A functor allows you to have a map function for free, althought that's not a compelling reason for why
   *   this concept exist.
   */
  trait Functor[F[_]]{
    def map[A, B](a: F[A])(f: A => B): F[B]

    /** Taking a step back, and trying to find out more combinators and helper functions with these functor instances.
      * being a bit verbose here when compared to textbook for better understanding.
      * That operation is sometimes called unzip. So we just wrote a generic
      * unzip function that works not just for lists, but for any functor!
      */
    def distribute[A, B](a: F[(A, B)]): (F[A], F[B]) = {
      val s: F[A] = map(a)(tuple => tuple._1)
      val y: F[B] = map(a)(tuple => tuple._2)
      (s, y)
    }

    // Co-distribute is just the opposite of this functionality
    def codistribute[A, B](a: Either[F[A], F[B]]): F[Either[A, B]] = {
      a match {
        case Right(fb) => map(fb)(Right(_))
        case Left(fa) => map(fa)(Left(_))
      }
    }
    /** Is functor that powerful. Not really!
      * For example the following will throw compilation error.
      * Guess, what this function intends to do:
      * def codistribute[A, B](fa: F[A], fb: F[B]): F[(A, B)] = {
      *   map(fa)(a => map(fb)(b => (a, b)))
      * }
      */
  }

  // An example of a functor instance
  val listFunctor = new Functor[List] {
    def map[A, B](a: List[A])(f: A => B) = a.map(f)
  }

  /**
   * Functor Laws
   * Example for Par data type
   * if x = Par(something)
   * map(x)(a => a) == x
   * map(x) preserves the structure
   * This kind of alge- braic reasoning can potentially save us a lot of work, s
   * ince we don’t have to write sepa- rate tests for these properties.
   */


  /** MONADS:
    * We defined functor, and possible magical combinators. However, a further upgrade
    * in terms of abstraction is needed, allowing you to do more fantastic things.
    * Here, we generalise flatMap and unit functions.
    *
    * def map2[A, B, C] (fa: Gen[A], fb: Gen[B])(f: (A, B) => C): Gen[C] - {
    *   fa flatMap (a => fb map (b => f(a,b)))
    * }
    *
    * The above implementation is similar for Parser, Option etc (replace Gen with Parser, Option)
    * This simply means you need to generalise the tyoe (Ex: Gen with Some F
    * Hope you remember map can be implemented in terms of flatMap and unit. Hence all monad instances
    * should implement a flatMap and unit. (That is the starting point - under
    *
    * At this point, what you get out of monad is just map2 and map function. You still don't know the big
    * picture of Monad. However you have already gained a significant knowledge on Monad.
    */

  trait Monad[F[_]] {
    def unit[A](a: => A): F[A]
    def flatMap[A, B](ma: F[A])(f: A => F[B]): F[B]

    // map can be implemented in terms of flatMap
    def map[A, B](ma: F[A])(f: A => B): F[B] = flatMap(ma)(a => unit(f(a)))
    def map2[A, B, C](ma: F[A], mb: F[B])(f: (A, B) => C): F[C] = flatMap(ma)(a => map(mb)(b => f(a, b)))
  }

  // To tie this back to a concrete data type, we can implement the Monad instance for Gen.
  object Monad {
    def genMonad = new Monad[Gen] {
      def unit[A](a: => A) = Gen.unit(a)
      def flatMap[A, B](ma: Gen[A])(f: A => Gen[B]): Gen[B] = ma.flatMap(f)
    }
  }
}