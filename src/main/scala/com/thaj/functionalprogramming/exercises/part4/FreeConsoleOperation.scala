package com.thaj.functionalprogramming.exercises.part4

import com.thaj.functionalprogramming.exercises.part3.MonadLearnings.Monad
import com.thaj.functionalprogramming.exercises.part3.Reader
import com.thaj.functionalprogramming.exercises.part4.FreeMonad.{ Free, Return, Suspend, ~> }

trait FreeConsoleOperation[A] {
  def toReader: Reader[Unit, A]
}

//boiler plates

object FreeConsoleOperation {

  case object ReadLine extends FreeConsoleOperation[String] {
    override def toReader: Reader[Unit, String] = Reader(_ => scala.io.StdIn.readLine())
  }

  case class PrintLine(string: String) extends FreeConsoleOperation[Unit] {
    override def toReader: Reader[Unit, Unit] = Reader(_ => println(string))
  }

  def readLine[A]: Free[FreeConsoleOperation, String] =
    Suspend(ReadLine)

  def printLine[A](s: String): Free[FreeConsoleOperation, Unit] =
    Suspend(PrintLine(s))

  val f: Free[FreeConsoleOperation, String] = for {
    _ <- printLine("I interact with only console")
    s <- readLine
  } yield s

  val translator = new (FreeConsoleOperation ~> Reader[Unit, ?]) {
    def apply[A](a: FreeConsoleOperation[A]): Reader[Unit, A] = a.toReader
  }

  // Scalaz already has a monad instance for Reader, however, our free monad runner expect an instance for our in-house monad
  implicit def readerMonad: Monad[Reader[Unit, ?]] = Reader.readerMonad[Unit]

  def reader: Reader[Unit, String] = FreeMonad.runFree[FreeConsoleOperation, Reader[Unit, ?], String](f)(translator)

  // reader.run("Bob") and you test your flow f nicely without any side effect. This is a rather simple
  // way of saying that the tranlsated F should be pure like a Reader Monad that removes the side effect.
  // The actual side effect may be handled out side. Something like, scalaz.IO(readLine()).map( reader.run) etc.
}
