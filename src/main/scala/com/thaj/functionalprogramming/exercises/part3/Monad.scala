package com.thaj.functionalprogramming.exercises.part3

import com.thaj.functionalprogramming.example.exercises.{Empty, Stream}
import com.thaj.functionalprogramming.example.exercises.part2.{Par, Gen}
import com.thaj.functionalprogramming.example.exercises.part2.Par.Par
import scala.{Stream => _}
/**
 * Created by afsalthaj on 4/03/17.
 */
object Monad {

  /**
   *   In all our previous APIs, we ended up using primitive functions such as map,
   *   flatMap etc and corresponding combinators
   *   such as sequence, traverse etc. If you haven't got a sense of these functions yet,
   *   its good to have a revisit on part 1, part 2
   *   etc
   *
   *   We will call F (or F[_] as) type constructor.
   */

  /**
   *   A functor allows you to have a map function for free, althought that's not a compelling
   *   reason for why this concept exist.
   */
  trait Functor[F[_]]{
    def map[A, B](a: F[A])(f: A => B): F[B]

    /** Taking a step back, and trying to find out more combinators and helper functions with these
      * functor instances.
      * Being a bit verbose here when compared to textbook for better understanding.
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
    def map[A, B](a: List[A])(f: A => B): List[B] = a.map(f)
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

  trait Monad[F[_]] extends  Functor[F] {
    def unit[A](a: => A): F[A]
    def flatMap[A, B](ma: F[A])(f: A => F[B]): F[B]

    // map can be implemented in terms of flatMap
    def map[A, B](ma: F[A])(f: A => B): F[B] = flatMap(ma)(a => unit(f(a)))
    def map2[A, B, C](ma: F[A], mb: F[B])(f: (A, B) => C): F[C] = flatMap(ma)(a => map(mb)(b => f(a, b)))


    /**
     * The sequence and traverse combinators should be pretty
     * familiar to you by now, and your implementations of them from
     * various prior chapters are probably all very simi- lar. Implement them once
     * and for all on Monad[F].
     * def sequence[A](lma: List[F[A]]): F[List[A]]
     * def traverse[A,B](la: List[A])(f: A => F[B]): F[List[B]
     */
    def sequence[A](lma: List[F[A]]): F[List[A]] = traverse(lma)(identity)
    def traverse[A,B](la: List[A])(f: A => F[B]): F[List[B]] =
      la.foldRight(unit(Nil): F[List[B]])((a, b) => map2(f(a), b)(_ :: _))
    /**
     * One combinator we saw for Gen and Parser was listOfN,
     * which allowed us to repli- cate a parser or generator n times to get a parser or
     * generator of lists of that length. We can implement this combinator for all monads F by
     * adding it to our Monad trait. We should also give it a more generic name such as
     * replicateM (meaning “replicate in a monad”).
     * Implement replicateM.
     */
    def replicateM[A](n: Int, ma: F[A]): F[List[A]] = sequence(List.fill(n)(ma))

    // We did this product for Gen, Par etc
    def product[A,B](ma: F[A], mb: F[B]): F[(A, B)] = map2(ma, mb)((_, _))

    // EXERCISE 11.6
    // Hard: Here’s an example of a function we haven’t seen before. Implement the function filterM.
    // It’s a bit like filter, except that instead of a function from A => Boolean,
    // we have an A => F[Boolean]. (Replacing various ordinary functions like this
    // with the monadic equivalent often yields interesting results.) Implement this function,
    // and then think about what it means for various data types.
    def filterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] = {
      val result: F[List[(A, Boolean)]] =
        ms.foldRight(unit(Nil): F[List[(A, Boolean)]])((a, b) =>
          map2(product(unit(a), f(a)), b)((c, d) =>
            if(c._2) c :: d else d
          ))

      map(result)(_.map(_._1))
    }
  }

  // To tie this back to a concrete data type, we can implement the Monad instance for Gen.
  object Monad {
    def genMonad = new Monad[Gen] {
      def unit[A](a: => A) = Gen.unit(a)
      def flatMap[A, B](ma: Gen[A])(f: A => Gen[B]): Gen[B] = ma.flatMap(f)
    }

    // Exercise 11.1
    // Write monad instances for Par, Parser, Option, Stream, and List.
    def parMonad = new Monad[Par]{
      def unit[A](a: => A): Par[A] = Par.unit(a)
      def flatMap[A, B](ma: Par[A])(f: A => Par[B]): Par[B] = Par.flatMapUsingJoin(ma)(f)
    }

    def optionMonad = new Monad[Option]{
      def unit[A](a: => A): Option[A] = Some(a)
      def flatMap[A, B](a: Option[A])(f: A => Option[B]) = a.flatMap(f)
    }

    def streamMonad = new Monad[Stream] {
      def unit[A](a: => A): Stream[A] = Stream.cons(a, Empty)
      def flatMap[A, B](a: Stream[A])(f: A => Stream[B]): Stream[B] = a.flatMap(f)
    }

    def listMonad = new Monad[List]{
      def unit[A](a: => A): List[A] = List(a)
      def flatMap[A, B](a: List[A])(f: A => List[B]): List[B] = a.flatMap(f)
    }
  }

  /**
   * 11.4
   * Think about how replicateM will behave for various choices of F. For example, how does
   * it behave in the List monad?
   * What about Option? Describe in your own words the general meaning of replicateM.
   */
  // Refer MonadSpec
}
