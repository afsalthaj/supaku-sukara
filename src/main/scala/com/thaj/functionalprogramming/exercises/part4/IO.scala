package com.thaj.functionalprogramming.exercises.part4

import com.thaj.functionalprogramming.exercises.part3.MonadLearnings.Monad

import scala.annotation.tailrec
import scala.io.StdIn

/**
  * Created by afsalthaj on 1/05/2017.
  */
sealed trait IO[A]{ self =>
  def run: A
  def map[B](f: A => B): IO[B] = new IO[B]{def run = f(self.run)}
  def flatMap[B](f: A => IO[B]): IO[B] = new IO[B] { def run = f(self.run).run }
}


object IOOperations {

  implicit object IO extends Monad[IO] {
    def unit[A](a: => A): IO[A] = new IO[A]{ def run = a}
    def flatMap[A, B](ma: IO[A])(f: (A) => IO[B]): IO[B] = ma flatMap f
    def apply[A](a: => A): IO[A] = unit(a)
  }

  def ReadLine = IO { StdIn.readLine }
  def PrintLine(msg: String) = IO { println(msg) }

  def fahrenheitToCelsius(f: Double): Double = (f - 32) * 5.0/9.0

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
    if(n != 0) inner(1, n) else 0
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
    _  <- PrintLine(factorialN(aa).toString)
    _ <- if (aa!=0) continuouslyDoReadLineAndFindFactorial else m.unit()
  } yield ()
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

  @tailrec
  def run[A](io: IO[A]): A = io match {
    case Return(a) => a
    case Suspend(r) => r()
    case FlatMap(x, f) => x match {
      case Return(a) => run(f(a))
      case Suspend(r) => run(f(r()))
      case FlatMap(y, g) => run(y flatMap(a => g(a) flatMap f))
    }
  }

  case class Return[A](a: A) extends IO[A]
  case class Suspend[A](resume: () => A) extends  IO[A]
  case class FlatMap[A, B](bs: IO[A], f: A => IO[B]) extends IO[B]

  def PrintLine(msg: String): IO[Unit] = Suspend(() => Return(println(msg)))

  // the one that cannot result in stack overflow
  def factorialREPIOWithoutHeap(implicit m: Monad[IO]): IO[List[Unit]] =
    m.sequence((0 to 100000).map(t => PrintLine(IOOperations.factorialN(t).toString)).toList)
}