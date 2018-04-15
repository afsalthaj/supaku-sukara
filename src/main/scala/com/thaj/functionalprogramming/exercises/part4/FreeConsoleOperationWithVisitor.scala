package com.thaj.functionalprogramming.exercises.part4

import com.thaj.functionalprogramming.exercises.part3.MonadLearnings.Monad
import com.thaj.functionalprogramming.exercises.part3.Reader
import com.thaj.functionalprogramming.exercises.part4.FreeMonad.{ Free, Return, Suspend, ~> }

object FreeConsoleOperationWithVisitor { module =>
  trait ConsoleOperation[A] {
    def visit[F[_]](visitor: ConsoleOperation.Visitor[F]): F[A]
  }

  type ScalazConsoleVisistorFree[A] = Free[ConsoleOperation, A]

  object ConsoleOperation {

    trait Visitor[F[_]] extends (ConsoleOperation ~> F) {
      def apply[A](fa: ConsoleOperation[A]): F[A] = fa.visit(this)

      def readLine: F[String]

      def printLine(string: String): F[Unit]

    }

    case object ReadLine extends ConsoleOperation[String] {
      def visit[F[_]](visitor: FreeConsoleOperationWithVisitor.ConsoleOperation.Visitor[F]): F[String] = visitor.readLine
    }

    case class PrintLine(s: String) extends ConsoleOperation[Unit] {
      def visit[F[_]](visitor: FreeConsoleOperationWithVisitor.ConsoleOperation.Visitor[F]): F[Unit] = visitor.printLine(s)
    }
  }

  import ConsoleOperation._

  val unit: ScalazConsoleVisistorFree[Unit] = Return[ConsoleOperation, Unit](())
  def printLine(s: String): ScalazConsoleVisistorFree[Unit] = Suspend(PrintLine(s))
  def readLine: ScalazConsoleVisistorFree[String] = Suspend(ConsoleOperation.ReadLine)

  val f: Free[ConsoleOperation, String] = for {
    _ <- printLine("I interact with only console")
    s <- readLine
  } yield s

  // Scalaz already has a monad instance for Reader, however, our free monad runner expect an instance for our in-house monad
  implicit def readerMonad: Monad[Reader[Unit, ?]] = Reader.readerMonad[Unit]

  def reader: Reader[Unit, String] = FreeMonad.runFree[ConsoleOperation, Reader[Unit, ?], String](f)(
    new Visitor[Reader[Unit, ?]] {
      def readLine: Reader[Unit, String] = Reader(_ => scala.io.StdIn.readLine())
      def printLine(string: String): Reader[Unit, Unit] = Reader(_ => print(string))
    })
}
