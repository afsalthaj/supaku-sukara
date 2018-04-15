package com.thaj.functionalprogramming.exercises.part4

import com.thaj.functionalprogramming.exercises.part3.MonadLearnings.Monad
import com.thaj.functionalprogramming.exercises.part3.Reader
import com.thaj.functionalprogramming.exercises.part4.FreeMonad.{ Free, Return, Suspend, ~> }

trait ScalazConsole[A] {
  def toReader: Reader[Unit, A]
}

//boiler plates

object ScalazConsole {

  case object ReadLine extends ScalazConsole[String] {
    override def toReader: Reader[Unit, String] = Reader(_ => scala.io.StdIn.readLine())
  }

  case class PrintLine(string: String) extends ScalazConsole[Unit] {
    override def toReader: Reader[Unit, Unit] = Reader(_ => println(string))
  }

  def readLine[A]: Free[ScalazConsole, String] =
    Suspend(ReadLine)

  def printLine[A](s: String): Free[ScalazConsole, Unit] =
    Suspend(PrintLine(s))

  val f: Free[ScalazConsole, String] = for {
    _ <- printLine("I interact with only console")
    s <- readLine
  } yield s

  val translator = new (ScalazConsole ~> Reader[Unit, ?]) {
    def apply[A](a: ScalazConsole[A]): Reader[Unit, A] = a.toReader
  }

  // Scalaz already has a monad instance for Reader, however, our free monad runner expect an instance for our in-house monad
  implicit def readerMonad: Monad[Reader[Unit, ?]] = Reader.readerMonad[Unit]

  def reader: Reader[Unit, String] = FreeMonad.runFree[ScalazConsole, Reader[Unit, ?], String](f)(translator)

  // reader.run("Bob") and you test your flow f nicely without any side effect. This is a rather simple
  // way of saying that the tranlsated F should be pure like a Reader Monad that removes the side effect.
  // The actual side effect may be handled out side. Something like, scalaz.IO(readLine()).map( reader.run) etc.
}
