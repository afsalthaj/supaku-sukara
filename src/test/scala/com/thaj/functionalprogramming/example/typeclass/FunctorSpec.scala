package com.thaj.functionalprogramming.example

import com.thaj.functionalprogramming.example.typeclass.functor.Functor
import com.thaj.functionalprogramming.example.typeclass.functor.Functor.ops._
import com.thaj.functionalprogramming.example.typeclass.functor.Functor._
import org.specs2.Specification

/**
  * Created by afsalthaj on 12/11/2016.
  */
object FunctorSpec extends Specification {
 def is =
   s"""
     | test void $testListVoid
     | test map $testListMap
     | test listAs $testListAs
     | test comoose $testCompose
   """.stripMargin

  val testListMap = {
     val functor: Functor[List] = Functor[List]
     assert( functor.map(List(1,2,3))(_ + 1) == List(2, 3, 4) )
  }

  val testListVoid = {
     assert(List(1,2,3).void == List((), (), ()))
  }

  val testListAs = {
    assert(List(1,2,3).as(10) == List(10, 10, 10))
  }

  val testCompose = {

    // Please note Functor[List] is basically implicitly[Functor[List]]... We got rid of implicitly using Functor.ops._
    val composedFunctor = Functor[List] compose Functor[Option]
    assert(composedFunctor.map(List(Some(1), None, Some(2)))(_ + 1) == List(Some(2), None, Some(3)))
  }
}
