package com.thaj.functionalprogramming.example

import com.thaj.functionalprogramming.exercises.part3.Applicative
import org.specs2.Specification

import scala.collection.immutable.Stream.Empty


/**
 * Created by afsalthaj on 17/04/17.
 */
class ApplicativeSpec extends Specification {
  def is =
    s2"""
      $testStreamApplicative
    """
  def testStreamApplicative = {
    val s: Stream[List[Int]] = Applicative.streamApplicative.sequence(List.fill(2)(Stream.continually(1)))
    s.take(1).toList.flatten == List(1, 1)
  }
}