package com.thaj.functionalprogramming.cats

import cats.Semigroup

import cats.instances.all._
/**
  * Created by afsalthaj on 6/05/2017.
  */
// http://typelevel.org/cats/typeclasses.html
// http://typelevel.org/cats/typeclasses/semigroup.html
// An example of semigroup without using simulacrum, and with cats implicit Semigroup for Map[String, Int]
object CatsOperations {
  object OpsOperations {

    implicit val semigroupMap = Semigroup[Map[String, Int]]

    // good to have a sense of Ops trait
    trait Ops[A] {
      def self: A

      def typeClassInstance: Semigroup[A]

      def |++|(that: A): A = typeClassInstance.combine(self, that)
    }

    object ops {
      implicit def toConvertOpsInstance[A](x: A)(implicit m: Semigroup[A]) = new Ops[A] {
        def self = x

        override def typeClassInstance: Semigroup[A] = Semigroup[A]
      }
    }
  }

  import OpsOperations.ops._
  val s = Map("afsal" -> 2) |++| Map ("thaj" -> 2)
}
