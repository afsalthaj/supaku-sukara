package com.thaj.functionalprogramming.exercises.part4

/**
 * The property of IO monad is kind of generic, and we could
 * rename it to TailRec
 */
sealed trait TailRec[A] {
  import TailRec._
  def flatMap[B](f: A => TailRec[B]): TailRec[B] =
    FlatMap(this, f)
  def map[B](f: A => B): TailRec[B] =
    flatMap(f andThen (Return(_)))
}
object TailRec {
  case class Return[A](a: A) extends TailRec[A]
  case class Suspend[A](resume: () => A) extends TailRec[A]
  case class FlatMap[A, B](sub: TailRec[A], k: A => TailRec[B]) extends TailRec[B]
}
