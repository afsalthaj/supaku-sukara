package com.thaj.functionalprogramming.example

import org.scalacheck.Gen
import org.scalatest.prop.{Checkers, PropertyChecks}
import org.scalatest.{FlatSpec, Matchers, WordSpec}
import org.specs2.Specification

import scala.{Stream => _}
/**
  * Created by afsalthaj on 23/10/2016.
  */
class StreamSpec extends FlatSpec with Matchers {
  import com.thaj.functionalprogramming.example.exercises.Stream

  "A toList function over stream" should
    "give all the elements as a List " in {
      val stream = Stream ("a", "b" ,"c")
      assert( stream.toList == List("a", "b", "c"))
  }

  "A take function over stream" should
    "return the first n elements and return a string" in {
      val stream = Stream("a", "b", "c")
      assert (stream.take(1).toList == Stream("a").toList)
  }

  "A forall over stream" should
    "terminate if the condition is met for any string" in {
     val stream = Stream("a", "b", "c")
    assert (stream.exists(_ != "d"))
  }

  "A forall over stream" should
    "terminate if the condition is not met for any string" in {
    val stream = Stream("a", "b", "c")
    assert (stream.exists(_ != "d"))
  }

  "A forall over stream" should
    "should take all the elements until the given condition becomes false" in {
    val stream = Stream("a", "b", "c")
    assert (stream.takeWhile(_ != "b").toList == Stream("a").toList)
  }


  "A headoption using foldRight for stream" should
    "should optional head value" in {
    val stream = Stream("a", "b", "c")
    assert (stream.headOption_1.contains("a"))
  }
}