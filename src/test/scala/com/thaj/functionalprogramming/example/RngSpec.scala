package com.thaj.functionalprogramming.example

import com.thaj.functionalprogramming.example.exercises._
import PureStatefulAPI._
import org.scalatest.WordSpec
import org.scalatest.prop.{Checkers, PropertyChecks}
import org.specs2.Specification

class RngSpec extends WordSpec with PropertyChecks with Checkers {
  import com.thaj.functionalprogramming.example.exercises.PureStatefulAPI._

  "RNG getDouble functions" must {
    "have passed" in {
      forAll { (w: Long) => {
        val s = double(SimpleRng(w))._1
        assert(s >= 0 && s < 1.0)
      }
      }
    }
  }

  "int double function" must {
    "have passed" in {
      forAll { (w: Long) => {
        val s = intDouble(SimpleRng(w))._1
        assert(s._1 >=0  && s._2 < 1.0)
      }
      }
    }
  }
}
