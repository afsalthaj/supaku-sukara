package com.thaj.functionalprogramming.example

import com.thaj.functionalprogramming.example.exercises.PureStatefulAPI.{ RNG, SimpleRng }
import com.thaj.functionalprogramming.example.exercises.PureStatefulAPIGeneric.State
import com.thaj.functionalprogramming.exercises.part3.MonadLearnings.Monad
import org.specs2.Specification
import com.thaj.functionalprogramming.example.exercises.PureStatefulAPI._

import Monad._
/**
 * Created by afsalthaj on 18/03/17.
 */
object MonadSpec extends Specification {

  def is =
    s"""
    test list monad $testListMonad
    test option monad $testOptionMonad
    test filterM $testFilterM
    test join of monad $testJoin
    test state monad $testStateMonad
  """

  def testListMonad = {
    assert(listMonad.replicateM(4, List(1)) == List(List(1, 1, 1, 1)))
    assert(listMonad.replicateM(4, Nil) == Nil)
  }

  def testOptionMonad = {
    assert(optionMonad.replicateM(4, Some(1)).contains(List(1, 1, 1, 1)))
    assert(listMonad.replicateM(4, Nil) == Nil)
  }

  def testFilterM = {
    def resultBool(x: String) = if (x.length > 2) Some(true) else Some(false)

    assert(optionMonad.filterM(List("afsal", "thaj", "af"))(resultBool).contains(List("afsal", "thaj")))
  }

  def testJoin = assert(optionMonad.join(Some(Some(1))).contains(1))

  // Exercise 11.18
  // Now that we have a State monad, you should try it out to see how it behaves.
  // What is the meaning of replicateM in the State monad? How does map2 behave? What about sequence?
  def testStateMonad = {
    // What is the meaning of replicateM in the State monad?
    val replicatedState: State[RNG, List[Int]] = stateMonad.replicateM(2, State((s: RNG) => s.nextInt))
    // How does map2 behave?
    val map2State: State[RNG, Double] =
      stateMonad.map2(State((s: RNG) => s.nextInt), State(double3))((a, b) => a + b._1 + b._2 + b._3)

    val sequenceState: State[RNG, List[Double]] = stateMonad.sequence(List(State(double), State(double)))

    assert(replicatedState.run(SimpleRng(1)) === State((s: RNG) => {
      val (i1, s1) = s.nextInt
      val (i2, s2) = s1.nextInt
      (List(i1, i2), s2)
    }).run(SimpleRng(1)))

    assert(map2State.run(SimpleRng(1)) === State((s: RNG) => {
      val (i1, s1) = s.nextInt
      val ((d1, d2, d3), s2) = double3(s1)
      (i1 + d1 + d2 + d3, s2)
    }).run(SimpleRng(1)))

    assert(sequenceState.run(SimpleRng(1)) === State((s: RNG) => {
      val (d1, s1) = double(s)
      val (d2, s2) = double(s1)
      (List(d1, d2), s2)
    }).run(SimpleRng(1)))
  }
}