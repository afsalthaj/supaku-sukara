package com.thaj.functionalprogramming.exercises.part4

import com.thaj.functionalprogramming.exercises.part4.FreeMonad.{ Free, Suspend, ~> }
import FreeMonad._
import com.thaj.functionalprogramming.exercises.part4.PureInterpreter.ConsoleReader

/**
 * Function0 is not just the simplest possible choice for the type parameter F, but also
 * one of the least restrictive in terms of what’s allowed. This lack of restriction gives us
 * no ability to reason about what a value of type Function0[A] might do. A more restrictive
 * choice for F in Free[F,A] might be an algebraic data type that only models interaction
 * with the console.
 */
sealed trait Console[A] {
  def toThunk: () => A
  // Try to understand toReader after u read PureInterpreter
  def toReader: ConsoleReader[A]
}

case object ReadLine extends Console[Option[String]] {
  def run: Option[String] = {
    try Some(readLine())
    catch {
      case e: Exception => None
    }
  }

  def toThunk: () => Option[String] = run _

  // Try to understand toReader after u read PureInterpreter
  def toReader = ConsoleReader[Option[String]](s => Some(s))
}

case class PrintLine(string: String) extends Console[Unit] {
  def toThunk: () => Unit = () => println(string)

  // Try to understand toReader after u read PureInterpreter
  def toReader: ConsoleReader[Unit] = ConsoleReader(_ => ())
}

/**
 * A Console[A] represents a computation that yields an A, but it’s restricted to one of two
 * possible forms: ReadLine (having type Console[Option[String]]) or PrintLine
 */
object Console {
  def readLn: Free[Console, Option[String]] =
    Suspend(ReadLine)

  def printLn(line: String): Free[Console, Unit] =
    Suspend(PrintLine(line))

  def f1: Free[Console, Option[String]] = for {
    _ <- printLn("I can only interact with the console.")
    ln <- readLn
  } yield ln

  // run the executin but this doesn't compile, because we need an implicit monad
  // or we want the operation to be a covariant functor theoretically. However it isn't possible
  /**
   * {{{
   *     def runF1: Console[Option[String]] = FreeMonad.run(f1)
   * }}}
   *
   * We must translate our Console type, which doesn’t form a monad, to some other type
   * (like Function0 or Par) that does. We’ll make use of the following type to do this
   * translation. This translation is defined in free monad. Let us the translate method to convert
   * to a console to Function0
   *
   * {{{
   *     def consoleToFunction0 = new Translate[Console, Function0] {
   * def apply[A](a: Console[A]): Function0[A] = a.toThunk
   * }
   * }}}
   */
  def consoleToFunction0 = new (Console ~> Function0) {
    def apply[A](a: Console[A]): Function0[A] = a.toThunk
  }

  def runf1 = FreeMonad.runFree(f1)(consoleToFunction0)

}