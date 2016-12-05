package com.thaj.functionalprogramming.example

import com.thaj.functionalprogramming.example.exercises.Exercise
import org.scalacheck.Gen
import org.scalatest.WordSpec
import org.scalatest.prop.{Checkers, PropertyChecks}
import org.scalatest.MustMatchers._


class PropertyBasedCheckingSpec extends WordSpec with PropertyChecks with Checkers {

  val intList = Gen.listOf(Gen.choose(0, 100))
  val property = forAll(intList)(ns => sum(ns.reverse) == sum(ns)) &&
    forAll(intList)(ns => if(ns.))
  import com.thaj.functionalprogramming.example.exercises.OptionOperation._
  "Option filter" must {
    "have passed" in {
      forAll { (w: String) =>
        whenever (w != "afsa") {
          Some("afsa").filter(_ == w) == None
        }
      }
    }
  }

  def sum(list: List[Int]): Int = list.sum
}