package com.thaj.functionalprogramming.example.typeclass

import simulacrum.{ op, typeclass }

/**
 * Created by afsalthaj on 27/10/2016.
 */
// This shows the use of simulacrum. Compare the code with Semigroup Simple
@typeclass trait Semigroup[A] {
  @op("|+|") def ap(a: A, b: A): A
}

object Semigroup {

  def generalAppend[A](a: A, b: A)(f: String => A) = f(s"$a$b")

  implicit val StringSemigroup = new Semigroup[String] {
    def ap(a: String, b: String) = generalAppend(a, b)(identity)
  }

  implicit val IntSemigroup = new Semigroup[Int] {
    def ap(a: Int, b: Int) = generalAppend(a, b)(_.toInt)
  }
}

object Simple {
  import Semigroup.ops._
  def something(a: Int, b: Int) = a.|+|(b)
}