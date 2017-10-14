package com.thaj.functionalprogramming.exercises.part4

/**
 * Function0 is not just the simplest possible choice for the type parameter F, but also
 * one of the least restrictive in terms of what’s allowed. This lack of restriction gives us
 * no ability to reason about what a value of type Function0[A] might do. A more restrictive
 * choice for F in Free[F,A] might be an algebraic data type that only models interaction
 * with the console.
 * @tparam A
 */
sealed trait Console[A] {
  def toThunk: () => A
}

case object ReadLine extends Console[Option[String]] {
  def run: Option[String] = {
    try Some(readLine())
    catch {
      case e: Exception => None
    }
  }

  def toThunk: () => Option[String] = run _
}

