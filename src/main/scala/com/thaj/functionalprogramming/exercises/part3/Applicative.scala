package com.thaj.functionalprogramming.exercises.part3

import com.thaj.functionalprogramming.example.exercises.PureStatefulAPIGeneric.State
import com.thaj.functionalprogramming.exercises.part3.MonadLearnings.{Functor, Monad}
import com.thaj.functionalprogramming.exercises.part3.MonoidExp.{Foldable, Monoid}

import scala.{Right => _}

import com.thaj.functionalprogramming.exercises.part3.MonadLearnings._

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

  /**
    * But traversable data types are too numerous for us to write specialized sequence and traverse methods
    * for each of them. What we need is a new interface. We’ll call it Traverse:
    *
    * {{{
       trait Traverse[F[_]] {
         def traverse[G[_]:Applicative,A,B](fa: F[A])(f: A => G[B]): G[F[B]] =
           sequence(map(fa)(f))
         def sequence[G[_]:Applicative,A](fga: F[G[A]]): G[F[A]] =
           traverse(fga)(ga => ga)
       }
    * }}}
    *
    * The interesting operation here is sequence. Look at its signature closely.
    * It takes F[G[A]] and swaps the order of F and G, so long as G is an applicative functor.
    * Now, this is a rather abstract, algebraic notion. We’ll get to what it all means in a minute, but first,
    * let’s look at a few instances of Traverse.
    */
  trait Traverse[F[_]] extends Functor[F] with Foldable[F]{ self =>
    def traverse[G[_]: Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]]
    // def sequence[A](lfa: List[F[A]]): F[List[A]] = traverse(lfa)(fa => fa)
    def sequence[G[_]: Applicative, A](f: F[G[A]]): G[F[A]] = traverse(f)(identity)

    // Exercise 12.14
    type Id[A] = A

    val applicativeInstance = new Applicative[Id] {
      def unit[A](a: => A): Id[A] = a
      def map2[A, B, C](a: Id[A], b: Id[B])(f: (A, B) => C): Id[C] = f(a, b)
    }

    // Uses of traverse
    // traverse is a generic version of map. hence we can define map, and there by foldMap
    // which would then make can make a Traverse `Foldable`
    // Exercise 12.14
    def map[A, B](fa: F[A])(f: A => B): F[B] = traverse[Id, A, B](fa)(f)(applicativeInstance)

    type ConstG[M, B] = M

    implicit def monoidApplicative[M](M: Monoid[M]) = new Applicative[({type f[x] = ConstG[M, x]}) #f] {
      def map2[A, B, C](a: M, b: M)(f: (A, B) => C): ConstG[M, C] = M.op(a, b)
      def unit[A](a: => A): ConstG[M, A] = M.zero
    }

    // This means that Traverse can extend Foldable and we can give a default implemen-
    // tation of foldMap in terms of traverse:
    def foldMap[A, M](as: F[A])(f: A => M)(mb: Monoid[M]): M =
      traverse[({type f[x] = ConstG[M, x]}) #f, A, Nothing](as)(f)(monoidApplicative(mb))

    // Exercise 12.17
    def foldRight[A,B](as: F[A])(z: B)(f: (A,B) => B): B = mapAccum(as, z)((a, b) => ((),f(a, b)))._2
    def foldLeft[A,B](as: F[A])(z: B)(f: (B,A) => B): B = mapAccum(as, z)((a, b) => ((), f(b, a)))._2

    def traverseS[S, A, B](fa: F[A])(f: A => State[S, B]): State[S, F[B]] =
      traverse[({type f[x] = State[S, x]}) #f, A, B](fa)(f)(Monad.stateMonad)

    def zipWithIndex[A](ta: F[A]): F[(A, Int)] = traverseS(ta)(a => {
      for {
        n <- Monad.getState[Int]
        _ <- Monad.setState(n + 1)
      } yield (a, n)
    }).run(0)._1

    override def toList[A](ta: F[A]): List[A] = traverseS(ta)(a => {
      for {
        list <- Monad.getState[List[A]]
        _ <- Monad.setState(a :: list)
      } yield list
    }).run(Nil)._2


    def mapAccum[S, A, B](ta: F[A], s: S)(f: (A, S) => (B, S)): (F[B], S) = traverseS(ta)(a => {
      for {
         s1 <- Monad.getState[S]
         (b, s2) = f(a, s1)
         _ <- Monad.setState(s2)
      } yield b
    }).run(s)

    // this one fails if both side are not the same
    def zip[A, B](fa: F[A], fb: F[B]): F[(A, B)] = mapAccum(fa, toList(fb))({
      case (a, Nil) => sys.error("there exists a value in either side which couldn't find a value on the either side to zip it with")
      case (a, x :: xs) => ((a, x), xs)
    })._1

    // A more flexible implementation of zip
    def zipL[A, B](fa: F[A], fb: F[B]): F[(A, Option[B])] = mapAccum(fa, toList(fb))({
      case (a, Nil) => ((a, None), Nil)
      case (a, x::xs) => ((a, Some(x)), xs)
    })._1

    def zipR[A, B](fa: F[A], fb: F[B]): F[(Option[A], B)] = mapAccum(fb, toList(fa))({
      case (b, Nil) => ((None, b), Nil)
      case (b, a::as) => ((Some(a), b), as)
    })._1

    def toListUsingMapAcc[A](ta: F[A]): List[A] = mapAccum(ta, Nil: List[A])((a, s) => ((), a :: s))._2.reverse
    def zipWithIndexUsingMapAccum[A](fa: F[A]): F[(A, Int)] = mapAccum(fa, 0)((a, s) => ((a, s), s + 1))._1


    // Exercise 12.18
    // Use applicative functor products to write the fusion of two traversals. This function will,
    // given two functions f and g, traverse fa a single time, collecting the results of both functions at once.
    // this is about fusing traversals
    def fuse[G[_],H[_],A,B](fa: F[A])(g: A => G[B], h: A => H[B]) (G: Applicative[G], H: Applicative[H]):
    (G[F[B]], H[F[B]]) = {
      // (traverse(fa)(f), traverse(fa)(g))
      // Please note that when we wanted to fuse it in a single go, and when we wanted to use traverse
      // to solve this problem, we had to give an applicative for `({type f[X] = (G[X], H[X])})#f` which is
      // given by G.productApp(H)
      traverse[({type f[X] = (G[X], H[X])})#f, A, B](fa)(a => (g(a), h(a)))(G.product(H))
    }

    /**
      * not only can we use composed applicative functors to fuse traversals, traversable functors themselves
      * compose. If we have a nested structure like Map[K,Option[List[V]]], then we can traverse the map, the option,
      * and the list at the same time and easily get
      * to the V value inside, because Map, Option, and List are all traversable.
      * you can traverse Map[K, Option[List[V]] and operate on V directly. That's awesome.
      * Get a sense of compose for `Applicatives`, `Functors` and even `Traverse`
      */
    // Exercise 12.19
    //Implement the composition of two Traverse instances
     def compose[G[_]](implicit G: Traverse[G]): Traverse[({type f[x] = F[G[x]]})#f] = new Traverse[({type f[x] = F[G[x]]})#f] {
       def traverse[M[_]: Applicative, A, B](fa: F[G[A]])(f: (A) => M[B]): M[F[G[B]]] = self.traverse(fa)((ga: G[A]) => G.traverse(ga)(f))
    }

    /**
     *
      scala> res0.toList(List(1,2,3))
      res1: List[Int] = List(1, 2, 3)

      scala> val x = List(1,2,3)
      x: List[Int] = List(1, 2, 3)

      scala> res0.toList(res0.reverse(x))
      res2: List[Int] = List(3, 2, 1)

      scala> val y = List(4,5,6)
      y: List[Int] = List(4, 5, 6)

      scala> res0.toList(res0.reverse(x)) ++ res0.toList(res0.reverse(y))
      res3: List[Int] = List(3, 2, 1, 6, 5, 4)

      scala> res0.reverse(res0.toList(y) ++ res0.toList(x))
      res4: List[Int] = List(3, 2, 1, 6, 5, 4)
     */
    // Different from fpinscala, but need to see whats the difference
    def reverse[A](fa: F[A]): F[A] = mapAccum(fa, ())((a, s) => (a, s))._1
  }

  // Exercise 12.13
  // Write Traverse instances for List, Option and Tree
  case class Tree[+A](head: A, tail: List[Tree[A]])

  val listTraverseInstance = new Traverse[List] {
    def traverse[G[_], A, B](fa: List[A])(f: A => G[B])(implicit m: Applicative[G]): G[List[B]] =
      fa.foldLeft(m.unit(Nil: List[B]))((acc, a) => m.map2(f(a), acc)(_ :: _))
  }

  val optionTraverseInstance = new Traverse[Option] {
    def traverse[G[_], A, B](fa: Option[A])(f: A => G[B])(implicit m: Applicative[G]): G[Option[B]] =
      fa match {
        case Some(a) => m.map(f(a))(Some(_))
        case None => m.unit(None)
      }
  }

  val treeTraverseInstance = new Traverse[Tree] {
    // slightly different from the original solution
    def traverse[G[_], A, B](fa: Tree[A])(f: (A) => G[B])(implicit m: Applicative[G]): G[Tree[B]] =
      m.map2(f(fa.head), listTraverseInstance.sequence(fa.tail.map(tt => traverse(tt)(f))))(Tree(_, _))
  }

  /**
    * A traversal is similar to a fold in that both take some data structure and apply a func- tion to the data within
    * in order to produce a result. The difference is that traverse preserves the original structure, whereas foldMap
    * discards the structure and replaces it with the operations of a monoid.
    * Look at the signature Tree[Option[A]] => Option[Tree[A]],
    * for instance. We’re preserving the Tree structure, not merely col- lapsing the values using some monoid.
    *
    * {{{
    *   def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B = {
    *     as.foldLeft(m.zero)((b: B, a: A) => m.op(b, f(a)))
    *   }
    * }}}
    */

  // Turning a Monoid into Applicative
  type Const[A] = Int
  def intMonoidApplicative(m: Monoid[Int]) = new Applicative[Const] {
    def map2[A, B, C](a: Const[A], b: Const[B])(f: (A, B) => C) = m.op(a, b)
    def unit[A](a: => A): Const[A] = m.zero
  }
}