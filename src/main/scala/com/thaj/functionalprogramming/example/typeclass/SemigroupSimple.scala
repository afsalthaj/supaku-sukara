package com.thaj.functionalprogramming.example.typeclass

// A Simple type class. The companion object consist of Ops
// consist extension methods for the value of type A, for which there is a SemiGroupSimple[A]
// intance.
trait SemigroupSimple[A] {
  def append(a: A, b: A): A
}

object SemigroupSimple {

  def generalAppend[A](a: A, b: A)(f:String => A) = f(s"$a$b")

  implicit object StringSemigroup extends SemigroupSimple[String] {
    def append(a: String, b: String) = generalAppend(a, b)(identity)
  }

  implicit object IntSemigroup extends SemigroupSimple[Int] {
    def append(a: Int, b: Int) = generalAppend(a, b)(_.toInt)
  }

  trait Ops[A] {
    def typeClassInstance: SemigroupSimple[A]
    def self: A
    def |+| (y: A): A = typeClassInstance.append(self, y)
  }

  object ops {
    implicit def toSemigroupOps[A](target: A)(
      implicit tc: SemigroupSimple[A]
    ): Ops[A] = new Ops[A]{
      def typeClassInstance: SemigroupSimple[A] = tc
      def self = target
    }
  }
}

//sample client pgm
object Sample {
  import SemigroupSimple.ops._
  def sum(a: Int, b: Int): Int = a |+| b
  def sum(a: String, b: String) = a |+| b
}