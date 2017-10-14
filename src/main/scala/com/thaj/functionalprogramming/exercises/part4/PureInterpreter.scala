package com.thaj.functionalprogramming.exercises.part4

import com.thaj.functionalprogramming.exercises.part3.MonadLearnings.Monad
import com.thaj.functionalprogramming.exercises.part4.FreeMonad.{ Free, ~> }

/**
 * A value of type Free[F,A] is like a program written in an instruction set provided by F.
 * In the case of Console, the two instructions are PrintLine and ReadLine. The recursive
 * scaffolding (Suspend) and monadic variable substitution (FlatMap and Return)
 * are provided by Free itself. We can introduce other choices of F for different instruction
 * sets, for example, different I/O capabilities—a file system F granting read/write
 * access (or even just read access) to the file system. Or we could have a network F
 * granting the ability to open network connections and read from them, and so on.
 */
object PureInterpreter {

  /**
   * A value of type Free[F,A] is like a program written in an instruction set provided by F.
   * In the case of Console, the two instructions are PrintLine and ReadLine. The recursive
   * scaffolding (Suspend) and monadic variable substitution (FlatMap and Return)
   * are provided by Free itself. We can introduce other choices of F for different instruction
   * sets, for example, different I/O capabilities—a file system F granting read/write
   * access (or even just read access) to the file system. Or we could have a network F
   * granting the ability to open network connections and read from them, and so on.
   */

  case class ConsoleReader[A](run: String => A) {
    def map[B](f: A => B): ConsoleReader[B] =
      ConsoleReader(r => f(run(r)))
    def flatMap[B](f: A => ConsoleReader[B]): ConsoleReader[B] =
      ConsoleReader(r => f(run(r)).run(r))
  }

  object ConsoleReader {
    implicit val monad = new Monad[ConsoleReader] {
      def unit[A](a: => A) = ConsoleReader(_ => a)
      def flatMap[A, B](ra: ConsoleReader[A])(f: A => ConsoleReader[B]) =
        ra flatMap f
    }
  }

  val consoleToReader = new (Console ~> ConsoleReader) {
    def apply[A](a: Console[A]): ConsoleReader[A] = a.toReader
  }

  def runConsoleReader[A](io: Free[Console, A]): ConsoleReader[A] =
    FreeMonad.runFree[Console, ConsoleReader, A](io)(consoleToReader)
}
