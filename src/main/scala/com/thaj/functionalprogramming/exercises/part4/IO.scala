package com.thaj.functionalprogramming.exercises.part4

import com.thaj.functionalprogramming.exercises.part3.MonadLearnings.Monad

import scala.annotation.tailrec
import scala.io.StdIn
import com.thaj.functionalprogramming.example.exercises.Stream
/**
 * Created by afsalthaj on 1/05/2017.
 */
sealed trait IO[A] { self =>
  def run: A
  def map[B](f: A => B): IO[B] = new IO[B] { def run = f(self.run) }
  def flatMap[B](f: A => IO[B]): IO[B] = {
    // you might do this; f(self.run) => that's  a blunder, you are directly
    // calling run when you try to chain different IO. The calling of `IO` should
    // be just a description of calling an IO
    new IO[B] { def run = f(self.run).run }
  }
}

object IOOperations {

  implicit object IO extends Monad[IO] {
    def unit[A](a: => A): IO[A] = new IO[A] { def run = a }
    def flatMap[A, B](ma: IO[A])(f: (A) => IO[B]): IO[B] = ma flatMap f
    def apply[A](a: => A): IO[A] = unit(a)
  }

  def ReadLine = IO { StdIn.readLine }
  def PrintLine(msg: String) = IO { println(msg) }

  def fahrenheitToCelsius(f: Double): Double = (f - 32) * 5.0 / 9.0

  def coverter: IO[Unit] = for {
    _ <- PrintLine("Enter a temperature in degree celsius")
    d <- ReadLine.map(_.toDouble)
    _ <- PrintLine(fahrenheitToCelsius(d).toString)
  } yield ()

  def factorial(n: Int): Long = {
    if (n <= 1) 1
    else n * factorial(n - 1)
  }

  // tail recursive factorial
  def factorialN(n: Int): Long = {
    def inner(acc: Long, nn: Long): Long = {
      if (nn <= 1L) acc
      else inner(acc * nn, nn - 1)
    }
    if (n != 0) inner(1, n) else 0
  }

  def factorialIO(implicit m: Monad[IO]): IO[Long] = for {
    _ <- PrintLine("Enter the factorial thing")
    result <- ReadLine.map(t => factorial(t.toInt))
  } yield result

  // this ask for 10 numbers and print out the factorial and list down the IO
  def factorialREPLIOFor3(implicit m: Monad[IO]): IO[List[Long]] = {
    m.replicateM(3, factorialIO)
  }

  // the one that can result in stack overflow
  def factorialREPIOHeap(implicit m: Monad[IO]): IO[List[Unit]] =
    m.sequence((0 to 100000).map(t => PrintLine(factorialN(t).toString)).toList)

  def continuouslyDoReadLineAndFindFactorial(implicit m: Monad[IO]): IO[Unit] = for {
    aa <- ReadLine.map(_.toInt)
    _ <- PrintLine(factorialN(aa).toString)
    _ <- if (aa != 0) continuouslyDoReadLineAndFindFactorial else m.unit()
  } yield ()
}

object ForEverCombinator {
  def forever[F[_], A, B](a: F[A])(implicit m: Monad[F]): F[B] = {
    lazy val t: F[B] = forever(a)
    m.flatMap(a)(_ => t)
  }

  implicit val ioBasic = new Monad[IO] {
    def unit[A](a: => A): IO[A] = new IO[A] { def run = a }
    def flatMap[A, B](fa: IO[A])(f: A => IO[B]) = {
      fa.flatMap(f)
    }
  }

  // Must Read: defining p doesn't result in stack overflow error, however
  // calling run will invoke the stack overflow
  // In this chapter we will focus only on this
  // This method creates a new IO object whose run definition calls
  // run again before calling f. THis will keep building up nested run calls
  // on the stack and eventually overflow it. What can be done about this?
  val p = forever(IOOperations.PrintLine("still going on"))

  // OPTIONAL READ: If you try the below operation, you may get a stack overflow error
  // there is no `run` here, as defining the option monad is in a way in `run` state.
  def tryWithOption = forever[Option, Int, Int](Some(1))(Monad.optionMonad)

  // OPTIONAL READ: Oops fail for stream? It also results in stack overflow
  // Stackless scala is a topic that we can cover at some point:
  // http://blog.higher-order.com/assets/trampolines.pdf
  // there is no concept of `run` that will start interpreting your description
  // the interpretaion of description straight away happens here.
  def tryWithStream = forever[Stream, Int, Int](Stream.constantEfficient(1))(Monad.streamMonad)

  // OPTIONAL READ
  implicit val scalaStreamMonad = new Monad[scala.Stream] {
    def unit[A](a: => A): scala.Stream[A] = scala.Stream.continually(a)
    override def flatMap[A, B](ma: scala.Stream[A])(f: (A) => scala.Stream[B]): scala.Stream[B] = ma.flatMap(f)
  }
  // OPTIONAL READ: Unfortunately same; learn trampolining soon to be discussed
  def tryWithScalaStream = forever[scala.Stream, Int, Int](scala.Stream.continually(1))
}

/**
 * This is a better representation of avoiding stack overflow
 * illustrated in the book over the pages from 235 to 238
 */
object IOWithoutOverFlow {

  trait IO[A] {
    def flatMap[B](f: A => IO[B]): IO[B] = FlatMap(this, f)
    def map[B](f: A => B): IO[B] = flatMap(f andThen (Return(_)))
  }

  implicit object IOMonad extends Monad[IO] {
    def unit[A](a: => A): IO[A] = Return(a)
    def flatMap[A, B](ma: IO[A])(f: (A) => IO[B]): IO[B] = ma flatMap f
  }

  /**
   * there is a lots going on here.
   * General concept that is to be understood:
   * We encapsulated the control flow (refer IOOperations)
   * as pure data types. Hence when you try to describe
   * an infinite IO operations, it doesn't bother calling `run`
   * and build the stack. Instead it can return the data type immediately
   * and later when you define interpreter (that is the run method below)
   * you can pattern match and make sure that everything is called, such a way
   * that everything is tail recursive.
   */
  @tailrec
  def run[A](io: IO[A]): A = io match {
    case Return(a)  => a
    case Suspend(r) => r()
    case FlatMap(x, f) => x match {
      // we didn't do run(f(run(x)) to make things tail recursive
      case Return(a)     => run[A](f(a))
      case Suspend(r)    => run(f(r()))
      // OPTIONAL READ: we didn't go check if y is another FlatMap, instead we right
      // associated it using monadic associative law - and made sure it is tail recursive.
      // The below condition is basically, a FlatMap(FlatMap(y, g), f
      // However we made this into FlatMap(y, FlatMap(g,f)) so that next iteration
      // can pattern match on y and remain tail recursive.
      // So in short, we have a IO monad that deals with easier description
      // of programs and then have an interpreter at a later stage, that makes sure
      // that everything is tail recursive.
      case FlatMap(y, g) => run(y flatMap (a => g(a) flatMap f))
    }
  }

  case class Return[A](a: A) extends IO[A]
  case class Suspend[A](resume: () => A) extends IO[A]
  case class FlatMap[A, B](bs: IO[A], f: A => IO[B]) extends IO[B]

  def PrintLine(msg: String): IO[Unit] = Suspend(() => Return(println(msg)))

  // the one that cannot result in stack overflow
  // another simpler example is also given below using forever combinator
  def factorialREPIOWithoutStack(implicit m: Monad[IO]): IO[List[Unit]] =
    m.sequence((0 to 100000).map(t => PrintLine(IOOperations.factorialN(t).toString)).toList)

  // a new printLine IO operation
  def printLine(s: String): IO[Unit] = Suspend(() => Return(println(s)))

  // another simple example was calling forever
  val p = ForEverCombinator.forever(printLine("still going on"))(IOMonad)
  // calling run on this p doesn't result in any stack overflow error.

  /**
   * TAKE AWAY: It might blow your head => by encapsulating the control flow
   * using datastructures, we somehow avoided stack overflow error. How did this happen?
   * Refer to page 239, second paragraph in Red book.
   * http://blog.higher-order.com/assets/trampolines.pdf
   *
   * A function like run is sometimes called a trampoline and the overall technique
   * of returning control to a single loop to eliminate the stack is called trampolining.a
   */

  // A small correction in the red book, probably the author might have meant to use a FlatMap

  def foldLeft(list: List[Int => IO[Int]], init: Int => IO[Int])(f: (Int => IO[Int], Int => IO[Int]) => Int => IO[Int]): Int => IO[Int] = {
    @annotation.tailrec
    def go(innerList: List[Int => IO[Int]], acc: Int => IO[Int]): Int => IO[Int] =
      innerList match {
        case Nil     => acc
        case x :: xs => go(xs, f(acc, x))
      }

    go(list, init)
  }

  def f: (Int) => IO[Int] = (x: Int) => Return(x + 2)

  def g = foldLeft(List.fill(10000)(f), f)((acc, f) => {
    x => Suspend(() => ()).flatMap(_ => acc(x).flatMap(f))
  })

  /**
   *  Note: we could write a little helper function to make this nicer:
   *  def suspend[A](a: => IO[A]) = Suspend(() => ()).flatMap { _ => a }
   *   val g = List.fill(100000)(f).foldLeft(f) {
   *   (a, b) => x => suspend { a(x).flatMap(b) }
   *   }
   */

}

/**
 * the property of IO monad is kind of generic, and we could
 * rename it to TailRec
 * @tparam A
 */
sealed trait TailRec[A] {
  import TailRec._
  def flatMap[B](f: A => TailRec[B]): TailRec[B] =
    FlatMap(this, f)
  def map[B](f: A => B): TailRec[B] =
    flatMap(f andThen (Return(_)))
}
object TailRec {
  case class Return[A](a: A) extends TailRec[A]
  case class Suspend[A](resume: () => A) extends TailRec[A]
  case class FlatMap[A, B](sub: TailRec[A], k: A => TailRec[B]) extends TailRec[B]
}

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
}