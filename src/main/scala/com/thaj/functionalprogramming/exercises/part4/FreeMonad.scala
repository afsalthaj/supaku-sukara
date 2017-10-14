package com.thaj.functionalprogramming.exercises.part4

import com.thaj.functionalprogramming.exercises.part3.MonadLearnings.Monad
import com.thaj.functionalprogramming.exercises.part4

/**
 * A more nuanced IO type
 * During execution, the run interpreter will look at a TailRec program such as
 * FlatMap(Suspend(s),k), in which case the next thing to do is to call s(). The program
 * is returning control to run, requesting that it execute some effect s, wait for the
 * result, and respond by passing the resulting value to k (which may subsequently return
 * a further request). At the moment, the interpreter can’t know anything about what
 * kind of effects the program is going to have. It’s completely opaque. So the only thing it
 * can do is call s(). Not only can that have an arbitrary and unknowable side effect,
 * there’s no way that the interpreter could allow asynchronous calls if it wanted to. Since
 * the suspension is a Function0, all we can do is call it and wait for it to complete
 * Let make the Function0 generic. Let it be F
 * Means
 * type TailRec[A] = SomeStuff[Function0, A], and it has the property of monads, for free
 * type TailRec[A] = Free[F, A]
 */
object FreeMonad {
  sealed trait Free[F[_], A] {
    def flatMap[B](f: A => Free[F, B]): Free[F, B] =
      FlatMap(this, f)
    def map[B](f: A => B): Free[F, B] =
      flatMap(f andThen (Return(_)))
  }

  case class Return[F[_], A](a: A) extends Free[F, A]
  case class Suspend[F[_], A](s: F[A]) extends Free[F, A]
  case class FlatMap[F[_], A, B](s: Free[F, A], f: A => Free[F, B]) extends Free[F, B]

  /**
   * Exercise 13.1
   * Free is a monad for any choice of F. Implement map and flatMap methods on the
   * Free trait, and give the Monad instance for Free[F,_].
   * 10
   * def freeMonad[F[_]]: Monad[({type f[a] = Free[F,a]})#f]
   */
  def freeMonad[F[_]]: Monad[({ type f[a] = Free[F, a] })#f] = {
    new Monad[({ type f[a] = Free[F, a] })#f] {
      override def flatMap[A, B](ma: Free[F, A])(f: (A) => Free[F, B]): Free[F, B] =
        FlatMap(ma, f)

      override def unit[A](a: => A): Free[F, A] = Return(a)
    }
  }

  implicit def freeMonadInstance[F[_]] = freeMonad[F]

  /**
   * EXERCISE 13.2
   * Implement a specialized tail-recursive interpreter, runTrampoline, for running a
   * Free[Function0,A].
   */
  @annotation.tailrec
  def runTrampoline[A](a: Free[Function0, A]): A = a match {
    case Return(r)      => r
    case Suspend(thunk) => thunk()
    case FlatMap(sub, cont) => sub match {
      case Return(r)                 => runTrampoline(cont(r))
      case Suspend(thunk)            => runTrampoline(cont(thunk()))
      case FlatMap(subsub, contcont) => runTrampoline(subsub.flatMap(g => contcont(g) flatMap (cont)))
    }
  }

  // An example of using free monad to do the same computation
  type IO[Int] = Free[Function0, Int]

  def f: Int => IO[Int] = (x: Int) => Return(x + 2)
  // you can confidently run this method :D
  def g: (Int) => IO[Int] = List.fill(2)(f).foldLeft(f)((a, b) => x => Suspend(() => ()).flatMap(_ => a(x).flatMap(b)))

  // you have a specialised runTrampoline to run above g
  def runG = runTrampoline(g(10))

  /**
   * Exercise 13.3
   * Hard: Implement a generic interpreter for Free[F,A], given a Monad[F]. You can pattern
   * your implementation after the Async interpreter given previously, including use
   * of a tail-recursive step function.
   *
   * def run[F[_],A](a: Free[F,A])(implicit F: Monad[F]): F[A]
   * in the book, you can see a step function, which I am believing does the same thing
   */
  def run[F[_], A](a: Free[F, A])(implicit F: Monad[F]): F[A] = a match {
    case Return(v)   => F.unit(v)
    case Suspend(fa) => fa
    case FlatMap(sub, cont) => sub match {
      case Return(v)                 => run(cont(v))
      case Suspend(fa)               => F.flatMap(fa)(a => run(cont(a)))
      case FlatMap(subsub, contcont) => run(subsub.flatMap(g => contcont(g) flatMap (cont)))
    }
  }

  // you can make use of above run method to run g, but you might need to implement the monad instance of Function0
  implicit val function0Monad = new Monad[Function0] {
    def unit[A](a: => A) = () => a
    def flatMap[A, B](a: Function0[A])(f: A => Function0[B]) =
      () => f(a())()
  }

  def runGusingBetterRun = run(g(10)) // and that must give you 16

  /**
   * What is the meaning of Free[F,A]? Essentially, it’s a recursive structure that contains a
   * value of type A wrapped in zero or more layers of F.
   * 11 It’s a monad because flatMap
   * lets us take the A and from it generate more layers of F. Before getting at the result, an
   * interpreter of the structure must be able to process all of those F layers. We can view
   * the structure and its interpreter as coroutines that are interacting, and the type F
   * defines the protocol of this interaction. By choosing our F carefully, we can precisely control
   * what kinds of interactions are allowed.
   */
  /**
   * In a Free[F, A], F may not always be a monad, and we need
   * an implicit monad to run this free operation. To convert F into a target monad
   * we can have a translate function
   */
  trait Translate[F[_], G[_]] {
    def apply[A](a: F[A]): G[A]
  }

  type ~>[F[_], G[_]] = Translate[F, G]

  def runFree[F[_], G[_], A](a: Free[F, A])(t: F ~> G)(implicit G: Monad[G]): G[A] = a match {
    case Return(v)   => G.unit(v)
    case Suspend(fa) => t(fa)
    case FlatMap(sub, cont) => sub match {
      case Return(v)                 => runFree(cont(v))(t)
      case Suspend(fa)               => G.flatMap(t(fa))(a => runFree(cont(a))(t))
      case FlatMap(subsub, contcont) => runFree(subsub.flatMap(g => contcont(g) flatMap (cont)))(t)
    }
  }
}