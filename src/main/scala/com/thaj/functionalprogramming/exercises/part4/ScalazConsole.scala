package com.thaj.functionalprogramming.exercises.part4

import com.thaj.functionalprogramming.exercises.part3.MonadLearnings.Monad
import com.thaj.functionalprogramming.exercises.part4.FreeMonad.{ Free, Suspend, ~> }

import scalaz.Reader
import scalaz.syntax.std.option._

import scalaz.syntax.monad._
trait Logging[F]

object Logging {
  case class Info(msg: String) extends Logging
  case class Debug(msg: String) extends Logging
  case class Error(msg: String) extends Logging[Unit]

  def info(msg: String) = Info(msg)

  def urFun[F : scalaz.Monad](log: Logging ~> F) = {
    for {
      bla <- "bla".pure[F]
      r <- log(info("bla is lifted"))
      blabla <- "blabla".pure[F]
      _ <- log()
    } yield r
  }

  def info(msg: String): FreeMonad.Free[Logging, Unit] = Suspend(Info(msg))
}
/**
 * Scalaz Reader that we can use instead of defining a ConsoleReader[A]
 */
trait ScalazConsole[A] {
  def toReader: Reader[String, A]
}

//boiler plates

object ScalazConsole {

  case object ReadLine extends ScalazConsole[Option[String]] {
    override def toReader: Reader[String, Option[String]] = Reader(_.some)
  }

  case class PrintLine(string: String) extends ScalazConsole[Unit] {
    override def toReader: Reader[String, Unit] = Reader(r => println(r))
  }

  def readLine[A]: Free[ScalazConsole, Option[String]] =
    Suspend(ReadLine)

  def printLine[A](s: String): Free[ScalazConsole, Unit] =
    Suspend(PrintLine(s))

  val f: Free[ScalazConsole, Option[String]] = for {
    _ <- printLine("I interact with only console")
    s <- readLine
  } yield s

  val translator = new (ScalazConsole ~> Reader[String, ?]) {
    def apply[A](a: ScalazConsole[A]): Reader[String, A] = a.toReader
  }

  // Scalaz already has a monad instance for Reader, however, our free monad runner expect an instance for our in-house monad
  implicit def readerMonad: Monad[Reader[String, ?]] = new Monad[Reader[String, ?]] {
    override def flatMap[A, B](ma: Reader[String, A])(f: (A) => Reader[String, B]): Reader[String, B] = ma.flatMap(f)
    override def unit[A](a: => A): Reader[String, A] = Reader(_ => a)
  }

  def reader: Reader[String, Option[String]] = FreeMonad.runFree[ScalazConsole, Reader[String, ?], Option[String]](f)(translator)

  // reader.run("Bob") and you test your flow f nicely without any side effect. This is a rather simple
  // way of saying that the tranlsated F should be pure like a Reader Monad that removes the side effect.
  // The actual side effect may be handled out side. Something like, scalaz.IO(readLine()).map( reader.run) etc.
}
