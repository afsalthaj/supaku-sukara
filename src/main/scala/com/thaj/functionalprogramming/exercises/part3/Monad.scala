package com.thaj.functionalprogramming.exercises.part3

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
}