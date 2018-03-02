package com.thaj.functionalprogramming.exercises.part4

import com.thaj.functionalprogramming.exercises.part3.MonadLearnings.Monad

import scala.annotation.tailrec
import scala.io.StdIn
import com.thaj.functionalprogramming.example.exercises.Stream

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

  // this ask for 3 numbers and print out the factorial and list down the IO
  def factorialREPLIOFor3(implicit m: Monad[IO]): IO[List[Long]] = {
    m.replicateM(3, factorialIO)
  }

  // the one that can result in stack overflow
  def factorialREPIOStack(implicit m: Monad[IO]): IO[List[Unit]] =
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

  // defining p doesn't result in stack overflow error, however
  // calling run will invoke the stack overflow
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

  def PrintLine(n: Int): Suspend[Unit] = Suspend(() => println(IOOperations.factorialN(n)))

  // the one that cannot result in stack overflow
  // another simpler example is also given below using forever combinator
  def factorialREPIOWithoutStack(implicit m: Monad[IO]): IO[List[Unit]] = {
    val x = (0 to 100000).map(PrintLine(_)).toList
    m.sequence(x.toList)
  }

  // a new printLine IO operation
  def printLine(s: String): IO[Unit] = Suspend(() => println(s))

  // another simple example was calling forever
  val p = ForEverCombinator.forever(printLine("still going on"))(IOMonad)
  // calling run on this p doesn't result in any stack overflow error.
}