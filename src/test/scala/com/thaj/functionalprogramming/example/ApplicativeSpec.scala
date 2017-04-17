package com.thaj.functionalprogramming.example

import com.thaj.functionalprogramming.exercises.part3.Applicative
import org.specs2.Specification

/**
 * Created by afsalthaj on 17/04/17.
 */
class ApplicativeSpec extends Specification {
  def is =
    s2"""
      $testStreamApplicative
    """
  def testStreamApplicative = {
    Applicative.streamApplicative.sequence(List.fill(2)(Stream.continually(1))).toList == List(List(1, 1))
  }
}
