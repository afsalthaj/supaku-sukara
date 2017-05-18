package com.thaj.functionalprogramming.cats

import cats.{Applicative, Semigroup}

import scala.{specialized => sp}
import cats.instances.all._
/**
  * Created by afsalthaj on 6/05/2017.
  */
// http://typelevel.org/cats/typeclasses.html
// http://typelevel.org/cats/typeclasses/semigroup.html
// An example of semigroup without using simulacrum, and with cats implicit Semigroup for Map[String, Int]
// Concept 1
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

  // An example usage
  import OpsOperations.ops._
  def example = Map("afsal" -> 2) |++| Map ("thaj" -> 2)

  def combineOption[A : Semigroup](a: A, opt: Option[A]): A =
    opt.map(implicitly[Semigroup[A]].combine(a, _)).getOrElse(a)


  def mergeMap[K, V: Semigroup](lhs: Map[K, V], rhs: Map[K, V]) = lhs.foldLeft(rhs){case (acc, (k, v)) =>
    acc.updated(k, combineOption(v, acc.get(k)))
  }
}

// Concept 2
// A quick overview on specialised, which is widely used in cats
// http://www.scala-notes.org/2011/04/specializing-for-primitive-types/
trait CatsMonoid[@sp(Int, Long, Double, Float) A]

object CatsMonoid {
  val s  = new CatsMonoid[String] {}
}

// Concept 3
// for any Semigroup[A], there is a Monoid[Option[A]].
// http://typelevel.org/cats/typeclasses/monoid.html
object SemigroupAtoMonoidOptionOfA {
  import cats.Monoid
  import cats.data.NonEmptyList
  import cats.instances.option._

  val list = List(NonEmptyList(1, List(2, 3)), NonEmptyList(4, List(5, 6)))
  val lifted = list.map(nel => Option(nel))

  Monoid.combineAll(lifted)

}

// Concept 4
// Functors: Already covered in FP exercises
object FunctorCats{

  /**
    * object Functor {
    *   def apply[A: Functor]: Functor[A] = implicitly[Functor[A]]
    * }
    */
  import cats.Functor
  import cats.data.Nested
  import cats.syntax.functor._


  // An example usage of compose
  Functor[List].compose(Functor[Option]).map(List[Option[Int]]())(_ + 1)

  val nested: Nested[List, Option, Int] = Nested(List[Option[Int]]())
  // doesn't resolve in IDE
  nested.map(_ + 1)
}

// Concept 4
// Applicatives
object ApplicativeCats {
  import cats.instances.option._

  // Applicatives
  def traverseOption[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] =
  as.foldRight(Some(List.empty[B]): Option[List[B]]) { (a: A, acc: Option[List[B]]) =>
    val optB: Option[B] = f(a)
    // optB and acc are independent effects so we can use Applicative to compose
    Applicative[Option].map2(optB, acc)(_ :: _)
  }


  def traverse[F[_]: Applicative, A, B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldRight(Applicative[F].pure(List.empty[B])) { (a: A, acc: F[List[B]]) =>
      val fb: F[B] = f(a)
      Applicative[F].map2(fb, acc)(_ :: _)
    }
}

// Concept 5


