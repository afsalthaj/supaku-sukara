package com.thaj.functionalprogramming.exercises.part4

import com.thaj.functionalprogramming.exercises.part3.MonadLearnings.Monad

import scala.io.StdIn

/**
  * Created by afsalthaj on 1/05/2017.
  */

sealed trait IO[A]{ self =>
  def run: A
  def map[B](f: A => B): IO[B] = new IO[B]{def run = f(self.run)}
  def flatMap[B](f: A => IO[B]): IO[B] = new IO[B] { def run = f(self.run).run }
}

object IO extends Monad[IO] {
  def unit[A](a: => A): IO[A] = new IO[A]{ def run = a}
  def flatMap[A, B](ma: IO[A])(f: (A) => IO[B]): IO[B] = ma flatMap f
  def apply[A](a: => A): IO[A] = unit(a)
}

object IOOperations {
  def ReadLine = IO { StdIn.readLine }
  def PrintLine(msg: String) = IO { println(msg) }

  def fahrenheitToCelsius(f: Double): Double =
    (f - 32) * 5.0/9.0

  def coverter: IO[Unit] = for {
    _ <- PrintLine("Enter a temperature in degree celsius")
    d <- ReadLine.map(_.toDouble)
    _ <- PrintLine(fahrenheitToCelsius(d).toString)
  } yield ()
}