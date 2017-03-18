package com.thaj.functionalprogramming.example

import com.thaj.functionalprogramming.exercises.part3.Monad.Monad
import org.specs2.Specification

import Monad._
/**
 * Created by afsalthaj on 18/03/17.
 */
object MonadSpec extends Specification {

  def is=s"""
           $testListMonad
  """

  def testListMonad = {
    assert(listMonad.replicateM(4, List(1)) ==  List(List(1, 1, 1, 1)))
    assert(listMonad.replicateM(4, Nil) ==  Nil)
  }

  def testOptionMonad = {
    assert(optionMonad.replicateM(4, Some(1)).contains(List(1, 1, 1, 1)))
    assert(listMonad.replicateM(4, Nil) ==  Nil)
  }

  def testFilterM = {
    def resultBool (x: String) = if (x.length > 2) Some(true) else Some(false)

    val list = List("afsal", "thaj", "af")

    optionMonad.filterM(list)(resultBool).contains(List("afsal, thaj"))
  }
}
