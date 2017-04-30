package com.thaj.functionalprogramming.exercises.part3

import com.thaj.functionalprogramming.example.exercises.PureStatefulAPI.RNG
import com.thaj.functionalprogramming.example.exercises.PureStatefulAPIGeneric.State
import com.thaj.functionalprogramming.example.exercises.{Empty, Stream}
import com.thaj.functionalprogramming.example.exercises.part2.Par
import com.thaj.functionalprogramming.example.exercises.part2.Par.Par
import com.thaj.functionalprogramming.exercises.part2.Gen
import com.thaj.functionalprogramming.exercises.part3.Applicative.{Applicative, Traverse}

import scala.{Stream => _}

/**
 * Created by afsalthaj on 4/03/17.
 */
object MonadLearnings {

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
    def map[A, B](a: List[A])(f: A => B): List[B] = a match {
      case ::(x, xs) => ::(f(x), map(xs)(f))
      case Nil => Nil
    }
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

  trait Monad[F[_]] extends  Applicative[F] { self =>
    def unit[A](a: => A): F[A]
    def flatMap[A, B](ma: F[A])(f: A => F[B]): F[B]

    // map can be implemented in terms of flatMap
    override def map[A, B](ma: F[A])(f: A => B): F[B] = flatMap(ma)(a => unit(f(a)))
    def map2[A, B, C](ma: F[A], mb: F[B])(f: (A, B) => C): F[C] = flatMap(ma)(a => map(mb)(b => f(a, b)))

    /**
     * The sequence and traverse combinators should be pretty
     * familiar to you by now, and your implementations of them from
     * various prior chapters are probably all very simi- lar. Implement them once
     * and for all on Monad[F].
     * def sequence[A](lma: List[F[A]]): F[List[A]]
     * def traverse[A,B](la: List[A])(f: A => F[B]): F[List[B]
     */

    override def sequence[A](lma: List[F[A]]): F[List[A]] = traverse(lma)(identity)

    override def traverse[A, B](la: List[A])(f: A => F[B]): F[List[B]] =
      la.foldRight(unit(Nil): F[List[B]])((a, b) => map2(f(a), b)(_ :: _))
    /**
     * One combinator we saw for Gen and Parser was listOfN,
     * which allowed us to repli- cate a parser or generator n times to get a parser or
     * generator of lists of that length. We can implement this combinator for all monads F by
     * adding it to our Monad trait. We should also give it a more generic name such as
     * replicateM (meaning “replicate in a monad”).
     * Implement replicateM.
     */

    override def replicateM[A](n: Int, ma: F[A]): F[List[A]] = sequence(List.fill(n)(ma))

    // We did this product for Gen, Par etc
    override def product[A,B](ma: F[A], mb: F[B]): F[(A, B)] = map2(ma, mb)((_, _))

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

    // EXERCISE 11.7
    // Implement the Kleisli composition function compose.
    def compose[A,B,C](f: A => F[B], g: B => F[C]): A => F[C] = (a: A) => {
      flatMap(f(a))(bb => g(bb))
    }

    //￼EXERCISE 11.8
    // Hard: Implement flatMap in terms of compose. It seems that we’ve found
    // another minimal set of monad combinators: compose and unit.
    def _flatMap[A, B](ma: F[A])(f: A => F[B]): F[B] = {
      // take out a and apply f and u get F[F[B]]
      val z: Unit => F[A] = (a: Unit) => ma
      val k: (Unit) => F[B] = compose(z, f)
      k(())
    }

    // Exercise 11.12
    // There’s a third minimal set of monadic combinators: map, unit, and join.
    // Implement join in terms of flatMap.
    def join[A](mma: F[F[A]]): F[A] = {
      flatMap(mma)(identity)
    }

    // Exercise 11.13
    // Implement either flatMap or compose in terms of join and map.
    def __flatMap[A, B](ma: F[A])(f: A => F[B]): F[B] = {
       join(map(ma)(f))
    }

    // EXERCISE 12.11
    // Try to write compose on Monad. It’s not possible, but it is instructive to attempt it and
    // understand why this is the case.
    // def compose[G[_]](G: Monad[G]): Monad[({type f[x] = F[G[x]]})#f] = new Monad[({type f[x] = F[G[x]]})#f] {
    // def flatMap[A, B](ma: F[G[A]])(f: (A) => F[G[B]]): F[G[B]] = self.flatMap(ma)(ga => G.map(ga)(a => f(a)))
    // }


    // HARD
    // Exercise 12.20
    /**
      *
      * READ THIS ONLY AFTER YOU COMPLETE APPLICATIVES, COMPOSING APPLICATIVES, TRAVERSE ETC
      * Monad Composition
      * Let’s now return to the issue of composing monads.
      * As we saw earlier in this chapter, Applicative instances always compose,
      * but Monad instances do not. If you tried before to implement general monad composition,
      * then you would have found that in order to implement join for nested monads F and G, you’d have to write
      * something of a type like F[G[F[G[A]]]] => F[G[A]]. And that can’t be written generally. But if G also happens
      * to have a Traverse instance, we can sequence to turn G[F[_]] into F[G[_]], leading to F[F[G[G[A]]]].
      * Then we can join the adjacent F layers as well as the adjacent G layers using their respective Monad instances.
      *
      * Expressivity and power sometimes come at the price of compositionality and modular- ity.
      * The issue of composing monads is often addressed with a custom-written version of each monad that’s
      * specifically constructed for composition. This kind of thing is called a monad transformer.
      * For example, the OptionT monad transformer composes Option with any other monad:
      */
    def composeM[G[_]](F: Monad[F], G: Monad[G], T: Traverse[G]): Monad[({type f[x] = F[G[x]]})#f] =
      new Monad[({type f[x] = F[G[x]]})#f] {
       def flatMap[A, B](ma: F[G[A]])(f: (A) => F[G[B]]): F[G[B]] =
         F.map(F.join(F.map(ma)((g: G[A]) => T.traverse(g)(f)(F))))(a => G.join(a))
       override def unit[A](a: => A): F[G[A]] = F.unit(G.unit(a))
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

   /**
    * 11.4
    * Think about how replicateM will behave for various choices of F. For example, how does
    * it behave in the List monad?
    * What about Option? Describe in your own words the general meaning of replicateM.
    */
   // Refer MonadSpec

   /**
    * Monad Laws
    * In this section, we’ll introduce laws to govern our Monad interface.Certainly
    * we’d expect the functor laws to also hold for Monad, since a Monad[F]is a Functor[F],
    * but what else do we expect? What laws should constrain flatMap and unit?
    * The law is
    * x.flatMap(f).flatMap(g) == x.flatMap(a => f(a).flatMap(g))
    *
    * Example:
    * {{{
    *   x.flatMap(f).flatMap(g) == x.flatMap(a => f(a).flatMap(g))
    *   Some(v).flatMap(f).flatMap(g) == Some(v).flatMap(a => f(a).flatMap(g))
    *   f(v).flatMap(g) == (a => f(a).flatMap(g))(v)
    *   f(v).flatMap(g) == f(v).flatMap(g)
    * }}}
    * And this law should hold for all values x, f, and g of the appropriate types—not
    * just for Gen but for Parser, Option, and any other monad.
    * It’s not so easy to see that the law we just discussed is an associative law.
    * Remember the associative law for monoids? That was clear:
    * op(op(x,y), z) == op(x, op(y,z))
    *
    * But our associative law for monads doesn’t look anything like that!
    * Fortunately, there’s a way we can make the law clearer if we consider not the
    * monadic values of types like F[A], but monadic functions of types like A => F[B].
    * Functions like that are called Kleisli arrows,7 and they can be composed with one another
    * Refer compose function
    *
    * We can now state the associative law for monads in a much more symmetric way:
    * compose(compose(f, g), h) == compose(f, compose(g, h))
    */

   /**
    * 11.4.3 The identity laws
    * The other monad law is now pretty easy to see. Just like zero was an identity
    * element for append in a monoid, there’s an identity element for compose in a monad. Indeed,
    * that’s exactly what unit is, and that’s why we chose this name for this operation:8
    * def unit[A](a: => A): F[A]
    * This function has the right type to be passed as an argument to compose.
    * The effect should be that anything composed with unit is that same thing.
    * This usually takes the form of two laws, left identity and right identity:
    * compose(f, unit) == f
    * compose(unit, f) == f
    * We can also state these laws in terms of flatMap, but they’re less clear that way:
    * flatMap(x)(unit) == x
    * flatMap(unit(y))(f) == f(y)
    */

   /**
    * 11.5
    * Just what is monad?
    * Yes, Monad factors out code duplication among them,
    * but what is a monad exactly? What does “monad” mean?
    *
    * Monad, like Monoid, is a more abstract, purely algebraic interface.
    * The Monad combinators are often just a small fragment of the full API for a given data
    * type that happens to be a monad.
    *
    * So Monad doesn’t generalize one type or another; rather, many vastly different data types
    * can satisfy the Monad interface and laws.
    *
    * We’ve seen three minimal sets of primitive Monad combinators, and instances of Monad
    * will have to provide implementations of one of these sets:
    *  unit and flatMap
    *  unit and compose
    *  unit, map, and join
    *
    * And we know that there are two monad laws to be satisfied,
    * associativity and identity, that can be formulated in various ways.
    * monad is precisely defined by its operations and laws; no more, no less.
    * But it’s a little unsatisfying. It doesn’t say much about what it implies—what a monad means.
    * The problem is that it’s a self-contained definition. Even if you’re a beginning programmer,
    * you have by now obtained a vast amount of knowledge related to programming,
    * and this definition integrates with none of that.
    *
    * To develop some intuition for what monads mean, let’s look at another couple of monads
    * and compare their behavior.
    */

   // Identity Monad
   /**
    * To distill monads to their essentials, let’s look at the simplest interesting specimen,
    * the identity monad, given by the following type:
    */

   case class Id[A](value: A) {
     def map[B](f: A => B): Id[B] = Id(f(value))
     def flatMap[B](f: A => Id[B]): Id[B] = f(value)
   }

   /**
    * scala> Id("Hello, ") flatMap (a =>
    * |   Id("monad!") flatMap (b =>
    * |     Id(a + b)))
    * res0: Id[java.lang.String] = Id(Hello, monad!)
    * When we write the exact same thing with a for-comprehension, it might be clearer:
    * scala> for {
    *   a <- Id("Hello, ")
    *   b <- Id("monad!")
    * } yield a + b
    * res1: Id[java.lang.String] = Id(Hello, monad!)
    * So what is the action of flatMap for the identity monad?
    * It’s simply variable substitution. The variables a and b get bound to "Hello, " and "monad!",
    * respectively, and then substituted into the expression a + b. We could have written the same
    * thing without the Id wrapper, using just Scala’s own variables:
    * scala> val a = "Hello, "
    * a: java.lang.String = "Hello, "
    * scala> val b = "monad!"
    * b: java.lang.String = monad!
    * scala> a + b
    * res2: java.lang.String = Hello, monad!
    * Besides the Id wrapper, there’s no difference.
    *
    * So now we have at least a partial answer to the question of what monads mean.
    *
    * We could say that monads provide a context for introducing and binding variables,
    * and performing variable substitution.
    * Let’s see if we can get the rest of the answer.
    */
   object IdMonad extends Monad[Id]{
     def unit[A](a: => A): Id[A] = Id(a)
     def flatMap[A, B](ma: Id[A])(f: A => Id[B]): Id[B] = ma flatMap f
   }

   // 11.5.2 The State monad and partial type application
   // Visit State implementation
   type RandomNumberState[A] = State[RNG, A]
   // And IntState is exactly the kind of thing that we can build a Monad for:

   object IntStateMonad extends Monad[RandomNumberState] {
     def unit[A](a: => A): RandomNumberState[A] = State(s => (a, s))
     def flatMap[A,B](st: RandomNumberState[A])(f: A => RandomNumberState[B]): RandomNumberState[B] =
       st flatMap f
   }

   /**
    * Of course, it would be really repetitive if we had to manually write a
    * separate Monad instance for each specific state type. Unfortunately,
    * Scala doesn’t allow us to use underscore syntax to simply say State[Int, _] t
    * o create an anonymous type constructor like we create anonymous functions.
    * But instead we can use something similar to lambda syntax at the type level.
    * For example, we could have declared IntState directly inline like this:
    */
   object _IntStateMonad extends Monad[({
     type RandomNumberState[A] = State[RNG, A]
   }) # RandomNumberState ] {
     def unit[A](a: => A): RandomNumberState[A] = State(s => (a, s))
     def flatMap[A,B](st: RandomNumberState[A])(f: A => RandomNumberState[B]): RandomNumberState[B] =
       st flatMap f
   }

   def stateMonad[S] = new Monad[({
    type f[X] = State[S, X]
   }) # f] {
     def unit[A](a: => A): State[S, A] = State(s => (a, s))
     def flatMap[A, B](st: State[S, A])(f: A => State[S, B]): State[S, B] = {
       st flatMap f
     }
   }

    // Exercise 11.18
    // Refer Monad Spec
    /**
     *Let’s now look at the difference between the Id monad and the State monad.
     * Remember that the primitive operations on State (besides the monadic operations unit and flatMap)
     * are that we can read the current state with getState and we can set a new state with setState:
     * def getState[S]: State[S, S]
     * def setState[S](s: => S): State[S, Unit]
     * Remember that we also discovered that these combinators constitute a minimal
     * set of primitive operations for State. So together with the monadic primitives (unit and flatMap)
     * they completely specify everything that we can do with the State data type. This is true in general for
     * monads—they all have unit and flatMap, and each monad brings
     * its own set of additional primitive operations that are specific to it.
     *
     * For additional understanding, refer the code below (rivision)
     * {{{
     *       val mutatedState: State[RNG, Unit] = for {
     *         s <- State(PureStatefulAPI.double)
     *         y <- State.get
     *         _ <- State.set(y)
     *       } yield ()
     *       }
     * }}}
     */

    // Exercise 11.19
    // What laws do you expect to mutually hold for getState, setState, unit, and flatMap?
    // Refer MonadSpec
    def getState[S]: State[S, S] = State(s => (s, s))
    def setState[S](s: => S): State[S, Unit] = State( _ => ((), s))

    val F = stateMonad[Int]

    def unit[S, A](a: => A): State[S, A] = State(s => (a, s))

    def zipWithIndex[A](as: List[A]): List[(Int, A)] =
      as.foldLeft(F.unit(List[(Int, A)]()))((acc, a) => for {
        xs <- acc
        n  <- getState
        _  <- setState(n + 1)
      } yield (n, a) :: xs).run(0)._1.reverse

    // For better understanding, the above stuff is similar to the below code
    // This is where you kind of doing the same monadic operations, however, not using
    // the monadic trait, but directly using the flatMap and unit defined for state data type
    /**
     * scala> zipWithIndexWithoutForComp(List(2, 3, 4))
     * res7: List[(Int, Int)] = List((0, 2), (1, 3), (2, 4))
     */
    def zipWithIndexWithoutForComp[A](as: List[A]): List[(Int, A)] = as
      .foldLeft(unit[Int, List[(Int, A)]](Nil: List[(Int, A)]))((acc, a) => {
        acc.flatMap(xs => {
          State[Int, Int](n => (n, n + 1))
            .map(stateOfInt => (stateOfInt, a) :: xs)
        })
      }).run(0)._1.reverse
    /**
     * What does the difference between the action of Id and the action of State tell us
     * about monads in general? We can see that a chain of flatMap calls (or an equivalent
     * for-comprehension) is like an imperative program with statements that assign to vari- ables,
     * and the monad specifies what occurs at statement boundaries. For example, with Id, nothing at all
     * occurs except unwrapping and rewrapping in the Id constructor. With State, the most current state gets
     * passed from one statement to the next. With the Option monad, a statement may return None and terminate the
     * program. With the List monad, a statement may return many results, which causes statements that follow it to
     * potentially run multiple times, once for each result.
     * The Monad contract doesn’t specify what is happening between the lines, only that whatever
     * is happening satisfies the laws of associativity and identity.
     */
  }
}

// Exercise 11.20
/**
 * Hard: To cement your understanding of monads, give a monad instance for the follow- ing type,
 * and explain what it means. What are its primitive operations? What is the action of flatMap?
 * What meaning does it give to monadic functions like sequence, join, and replicateM? What meaning
 * does it give to the monad laws?11
 */
case class Reader[R, A](run: R => A)
object Reader {

  import MonadLearnings._

  def readerMonad[R] = new Monad[({type f[x] = Reader[R,x]})#f] {
    def unit[A](a: => A): Reader[R, A] = Reader(_ => a)
    def flatMap[A, B](st: Reader[R, A])(f: A => Reader[R, B]): Reader[R, B] = Reader (r => {
      val a: A = st.run(r)
      f(a).run(r)
    })
  }
}

// Take away from the chapter
// Monad: The algebraic interface of abstraction has combinators along with the primitives
// functions that act in a different way for different monad instances. For each monad, we may have different
// meanings for corresponding combinators/primitives, while all of them
// satisfy the monadic laws related to associativity and identity. The concern of logical coding
// is enormously reduced as well. The use of monad became significant in the case of state monad, where we were able to
// come up with a whole lot of functionalities apart from simple variable binding. A monadic operation written as
// for comprehension may have a whole of magic involved in between the written statements.This magic
// rely on the foundation of variable transition, variable binding, composition and so forth.
// When the types become monad, it becomes really for the developer to do composition of actions or types or results
// even if it is passing read only variable through various layers.
// We may have to revisit this chapter of monads and discover more meanings out of it.

// It seems Scala supports functional programming but not really a library completely based on FP concepts.
// Why not a monad trait in Scala? Or functor for that matter?
// Hence the existence of FP based libraries such as scalaz, shapeless etc become a value add in this context.
// I may get rid of this statement it is proving to be wrong in future.

// A small summary
// More inputs after revisiting the chapter: A summary
// A functor will have a map function, and the instances of the functor should implement map function. We will get functions
// such as distribute, codistribute etc for free. A functo can have a law map(x)(a => a) == x
// A monad instance should implement flatMap and unit function and you get functions such as map, map2, filterM, replicateM,
// sequence, traverse function for free. A monad law can be represented as:
// flatMap(fa)(fu).flatMap(ga)

// http://underscore.io/blog/posts/2015/04/28/monadic-io-laziness-makes-you-free.html

// now it follows substitutions


// A simple IO monad
/*
import Pure._
final case class Return[A](a: () => A) extends IO[A]
final case class Suspend[A](s: () => IO[A]) extends IO[A]

object Pure {
  trait IO[A] {
    def flatMap[B](a: A => IO[B]): IO[B] =
      Suspend(() => a(this.run))

    def map[B](f: A => B): IO[B] =
      Return(() => f(this.run))

    def run: A = this match {
      case Suspend(s) => s().run
      case Return(a) => a()
    }
  }

  object IO {
    def point[A](a: => A): IO[A] =
      Return(() => a)
  }

  def println(msg: String): IO[Unit] =
    IO.point(Predef.println(msg))
}

*/

object Pure {
  trait IO[A] {
    def flatMap[B](f: A => IO[B]): IO[B] = Suspend(() => f(this.run))
    def map[B](f: A => B): IO[B] = Return(() => f(this.run))

    def run: A= this match {
      case Suspend(s) => s().run
      case Return(r) => r()
    }
  }

  def point[A](a: A): IO[A] = Return(() => a)

  def println(msg: String): IO[Unit] = point(Predef.println(msg))
}

import Pure._

case class Return[A](a: () => A) extends IO[A]

case class Suspend[A](a: () => IO[A]) extends IO[A]