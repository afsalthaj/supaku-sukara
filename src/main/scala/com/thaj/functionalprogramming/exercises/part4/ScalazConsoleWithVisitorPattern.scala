package com.thaj.functionalprogramming.exercises.part4

import com.thaj.functionalprogramming.exercises.part3.MonadLearnings.Monad
import com.thaj.functionalprogramming.exercises.part3.Reader
import com.thaj.functionalprogramming.exercises.part4.FreeMonad.{ Free, Return, Suspend, ~> }

// An adoption of what's written in Doobie
object ScalazConsoleWithVisitorPattern { module =>
  trait ScalazConsoleVisitor[A] {
    def visit[F[_]](visitor: ScalazConsoleVisitor.Visitor[F]): F[A]
  }

  type ScalazConsoleVisistorFree[A] = Free[ScalazConsoleVisitor, A]

  object ScalazConsoleVisitor {

    trait Visitor[F[_]] extends (ScalazConsoleVisitor ~> F) {
      def apply[A](fa: ScalazConsoleVisitor[A]): F[A] = fa.visit(this)

      def readLine: F[String]

      def printLine(string: String): F[Unit]

    }

    case object ReadLine extends ScalazConsoleVisitor[String] {
      def visit[F[_]](visitor: ScalazConsoleWithVisitorPattern.ScalazConsoleVisitor.Visitor[F]): F[String] = visitor.readLine
    }

    case class PrintLine(s: String) extends ScalazConsoleVisitor[Unit] {
      def visit[F[_]](visitor: ScalazConsoleWithVisitorPattern.ScalazConsoleVisitor.Visitor[F]): F[Unit] = visitor.printLine(s)
    }
  }

  import ScalazConsoleVisitor._

  val unit: ScalazConsoleVisistorFree[Unit] = Return[ScalazConsoleVisitor, Unit](())
  def printLine(s: String): ScalazConsoleVisistorFree[Unit] = Suspend(PrintLine(s))
  def readLine: ScalazConsoleVisistorFree[String] = Suspend(ScalazConsoleVisitor.ReadLine)

  val f: Free[ScalazConsoleVisitor, String] = for {
    _ <- printLine("I interact with only console")
    s <- readLine
  } yield s

  // Scalaz already has a monad instance for Reader, however, our free monad runner expect an instance for our in-house monad
  implicit def readerMonad: Monad[Reader[Unit, ?]] = Reader.readerMonad[Unit]

  def reader: Reader[Unit, String] = FreeMonad.runFree[ScalazConsoleVisitor, Reader[Unit, ?], String](f)(
    new Visitor[Reader[Unit, ?]] {
      def readLine: Reader[Unit, String] = Reader(_ => scala.io.StdIn.readLine())
      def printLine(string: String): Reader[Unit, Unit] = Reader(_ => print(string))
    })
}
